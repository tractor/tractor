implode <- function (strings, sep = "", finalSep = NULL, ranges = FALSE)
{
    # Transform runs of integers into ranges
    # This is surprisingly tricky to get right!
    if (ranges && is.integer(strings))
    {
        # Perform run-length encoding on the differences between elements
        gapRunLengths <- rle(diff(strings))
        
        # Mark all elements not taken and find ranges (>1 consecutive unit difference)
        taken <- rep(FALSE, length(strings))
        withinRange <- gapRunLengths$values == 1 & gapRunLengths$lengths > 1
        
        # Convert range groups into strings, marking elements as taken to avoid double-counting
        rangeStrings <- lapply(which(withinRange), function(i) {
            # NB: Sum of a length-zero vector is zero
            start <- sum(gapRunLengths$lengths[seq_len(i-1)]) + 1
            end <- start + gapRunLengths$lengths[i]
            taken[start:end] <<- TRUE
            return (paste(strings[start], strings[end], sep="-"))
        })
        
        # Convert remaining elements into strings
        nonRangeStrings <- lapply(which(!withinRange), function(i) {
            start <- sum(gapRunLengths$lengths[seq_len(i-1)]) + 1
            end <- start + gapRunLengths$lengths[i]
            toKeep <- setdiff(start:end, which(taken))
            taken[toKeep] <<- TRUE
            return (as.character(strings)[toKeep])
        })
        
        # Arrange list of strings in the right order, and convert back to character vector
        strings <- vector("list", length(withinRange))
        strings[withinRange] <- rangeStrings
        strings[!withinRange] <- nonRangeStrings
        strings <- unlist(strings)
    }
    else
        strings <- as.character(strings)
    
    if (length(strings) == 1)
        return (strings[1])
    else if (length(strings) > 1)
    {
        result <- strings[1]
        for (i in 2:length(strings))
        {
            if (i == length(strings) && !is.null(finalSep))
                result <- paste(result, strings[i], sep=finalSep)
            else
                result <- paste(result, strings[i], sep=sep)
        }
        return (result)
    }
}

printLabelledValues <- function (labels, values, outputLevel = OL$Info, leftJustify = FALSE)
{
    if (length(labels) != length(values))
        report(OL$Error, "Labels and values should be of the same length")
    
    labelLengths <- nchar(labels)
    maxLabelLength <- max(labelLengths)
    nValues <- length(values)
    
    for (i in seq_len(nValues))
    {
        if (leftJustify)
            report(outputLevel, "  ", labels[i], implode(rep(" ",maxLabelLength-labelLengths[i]),sep=""), " : ", values[i], prefixFormat="")
        else
            report(outputLevel, implode(rep(" ",maxLabelLength-labelLengths[i]),sep=""), labels[i], " : ", values[i], prefixFormat="")
    }
    
    invisible(NULL)
}

relativePath <- function (path, referencePath)
{
    mainPieces <- strsplit(expandFileName(path), .Platform$file.sep, fixed=TRUE)[[1]]
    refPieces <- strsplit(expandFileName(referencePath), .Platform$file.sep, fixed=TRUE)[[1]]
    
    shorterLength <- min(length(mainPieces), length(refPieces))
    firstDifferentPiece <- min(which(mainPieces[1:shorterLength] != refPieces[1:shorterLength])[1], shorterLength, na.rm=TRUE)
    newPieces <- c(rep("..", length(refPieces)-firstDifferentPiece), mainPieces[firstDifferentPiece:length(mainPieces)])
    
    return (implode(newPieces, sep=.Platform$file.sep))
}

expandFileName <- function (fileName, base = getwd())
{
    fileName <- path.expand(fileName)
    
    # A leading slash, with (Windows) or without (Unix) a letter and colon, indicates an absolute path
    fileName <- ifelse(fileName %~% "^([A-Za-z]:)?/", fileName, file.path(base,fileName))
    
    # Remove all instances of '/.' (which are redundant), recursively collapse
    # instances of '/..', and remove trailing slashes
    fileName <- gsub("/\\.(?=/)", "", fileName, perl=TRUE)
    while (length(grep("/../", fileName, fixed=TRUE) > 0))
        fileName <- sub("/[^/]*[^./][^/]*/\\.\\.(?=/)", "", fileName, perl=TRUE)
    if (length(grep("/..$", fileName, perl=TRUE) > 0))
        fileName <- sub("/[^/]*[^./][^/]*/\\.\\.$", "", fileName, perl=TRUE)
    fileName <- gsub("/*\\.?$", "", fileName, perl=TRUE)
    
    return (fileName)
}

ensureFileSuffix <- function (fileName, suffix, strip = NULL)
{
    if (is.null(strip))
    {
        if (is.null(suffix))
            strip <- "\\w+"
        else
            strip <- suffix
    }
    else
        strip <- c(strip, suffix)
    
    stripPattern <- paste("\\.(", implode(strip,sep="|"), ")$", sep="")
    fileStem <- sub(stripPattern, "", fileName, ignore.case=TRUE, perl=TRUE)
    
    if (is.null(suffix))
        return (fileStem)
    else
    {
        fileName <- paste(fileStem, suffix, sep=".")
        return (fileName)
    }
}

locateExecutable <- function (fileName, errorIfMissing = TRUE)
{
    pathDirs <- unlist(strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed=TRUE))
    possibleLocations <- file.path(pathDirs, fileName)
    filesExist <- file.exists(possibleLocations)
    
    if (sum(filesExist) == 0)
    {
        if (errorIfMissing)
            report(OL$Error, "Required executable \"", fileName, "\" is not available on the system path")
        else
            return (NULL)
    }
    else
    {
        realLocations <- possibleLocations[filesExist]
        return (realLocations[1])
    }
}

execute <- function (executable, paramString = NULL, errorOnFail = TRUE, silent = FALSE, ...)
{
    execLoc <- locateExecutable(executable, errorOnFail)
    if (!is.null(execLoc))
    {
        execString <- paste(execLoc, paramString, sep=" ")
        if (silent && getOutputLevel() > OL$Debug)
            execString <- paste(execString, ">/dev/null 2>&1", sep=" ")
        report(OL$Debug, execString)
        system(execString, ...)
    }
}

promote <- function (x, byrow = FALSE)
{
    if (is.matrix(x))
        return (x)
    else if (is.numeric(x))
    {
        m <- as.matrix(x)
        if (byrow)
            m <- t(m)
        return (m)
    }
    else
        return (NA)
}

equivalent <- function (x, y, signMatters = TRUE, ...)
{
    if (signMatters)
        return (isTRUE(all.equal(x, y, ...)))
    else
        return (isTRUE(all.equal(abs(x), abs(y), ...)))
}

stripNul <- function (x, method = c("truncate","drop"))
{
    method <- match.arg(method)
    nul <- which(x == 0L)
    if (length(nul) == 0)
        return (x)
    else if (method == "truncate")
        return (x[seq_len(nul[1]-1)])
    else
        return (x[-nul])
}

threadSafeTempFile <- function (pattern = "file")
{
    tempDir <- file.path(tempdir(), paste("temp",Sys.getpid(),sep="_"))
    if (!file.exists(tempDir))
        dir.create(tempDir)
    return (tempfile(pattern=pattern, tmpdir=tempDir))
}
