"%~%" <- function (X, Y)
{
    if (!is.character(X) || !is.character(Y) || length(Y) != 1)
        output(OL$Error, "Parameters for matching on a regular expression are not valid")
    
    matchLocs <- regexpr(Y, X, perl=TRUE)
    return (!(matchLocs == -1))
}

implode <- function (strings, sep = "", finalSep = NULL)
{
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

expandFileName <- function (fileName)
{
    fileName <- path.expand(fileName)
    if (length(grep("^/", fileName)) == 0)
        fileName <- file.path(getwd(), fileName)
    
    # Remove all instances of '/.' (which are redundant), recursively collapse
    # instances of '/..', and remove trailing slashes
    fileName <- gsub("/\\.(?=/)", "", fileName, perl=TRUE)
    while (length(grep('/../', fileName, fixed=TRUE) > 0))
        fileName <- sub("/[^/]*[^./][^/]*/\\.\\.(?=/)", "", fileName, perl=TRUE)
    fileName <- gsub("/*\\.?$", "", fileName, perl=TRUE)
    
    return(fileName)
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
            output(OL$Error, "Required executable \"", fileName, "\" is not available on the system path")
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
        if (silent && getOption("tractorOutputLevel") > OL$Debug)
            execString <- paste(execString, ">/dev/null 2>&1", sep=" ")
        output(OL$Debug, execString)
        system(execString, ...)
    }
}

resolveVector <- function (len, ...)
{
    vector <- c(...)
    if (is.numeric(vector) && (length(vector) == len))
        return (vector)
    else
        return (NULL)
}

promote <- function (x, byrow = FALSE)
{
    if (is.matrix(x))
        return (x)
    else if (is.vector(x))
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
