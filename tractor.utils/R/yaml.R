readYaml <- function (fileName = NULL, text = NULL, init = list())
{
    trimWhitespaceAndQuotes <- function (x)
    {
        if (x %~% "^\\s*$")
            return (NULL)
        else
            return (ore.subst("^\\s*\"?(.*?)\"?\\s*$", "\\1", as.character(x)))
    }
    
    mapping <- init
    
    # Recursive cases: file and text, and/or multiple files
    if (!is.null(fileName))
    {
        fileNames <- unlist(strsplit(fileName, ":", fixed=TRUE))
        if (length(fileNames) > 1)
        {
            emptyLocs <- grep("^\\s*$", fileNames, perl=TRUE)
            if (length(emptyLocs) > 0)
                fileNames <- fileNames[-emptyLocs]
            
            for (file in fileNames)
                mapping <- readYaml(file, init=mapping)
            
            if (!is.null(text))
                mapping <- readYaml(text=text, init=mapping)
            
            return (mapping)
        }
        else if (!is.null(text))
        {
            # Text takes priority
            mapping <- readYaml(fileName, init=mapping)
            mapping <- readYaml(text=text, init=mapping)
            return (mapping)
        }
    }
    
    if (!is.null(fileName) && !file.exists(fileName))
        report(OL$Error, "YAML file ", fileName, " does not exist")
    
    if (!is.null(fileName))
    {
        lines <- readLines(fileName, warn=FALSE)
        usingFile <- TRUE
    }
    else if (!is.null(text))
    {
        lines <- unlist(strsplit(text, '\\s+', perl=TRUE))
        usingFile <- FALSE
    }
    else
        return (mapping)
    
    unlabelled <- character(0)
    repeat
    {
        if (length(lines) == 0)
            break
        
        # Ignore blank lines and those starting with the comment char, '#'
        if ((lines[1] %~% "^\\s*#") || (lines[1] %~% "^\\s*$"))
        {
            lines <- lines[-1]
            next
        }
        
        # Everything before the first colon in the line is the key; the rest is the associated value
        match <- ore.search("^\\s*([\\w-]+)\\s*:\\s*(.*?)\\s*$", lines[1])
        if (is.null(match))
        {
            if (usingFile)
                report(OL$Warning, "A line in the YAML file ", fileName, " has no key")
            else
                unlabelled <- c(unlabelled, trimWhitespaceAndQuotes(lines[1]))
            
            lines <- lines[-1]
            next
        }
        else
        {
            key <- match[,1]
            textValue <- match[,2]
            report(OL$Debug, "Key is ", key, "; value is ", textValue)
        }
        
        # Values starting with a square bracket are sequences (vectors in R)
        if (regexpr("[", textValue, fixed=TRUE) == 1)
        {
            endBracketLines <- setdiff(grep("]", lines, fixed=TRUE), grep("^\\s*#", lines, perl=TRUE))
            if (length(endBracketLines) == 0)
                report(OL$Error, "YAML syntax error: unmatched bracket")
            
            endLine <- endBracketLines[1]
            if (endLine > 1)
            {
                allLines <- setdiff(2:endLine, grep("^\\s*#", lines, perl=TRUE))
                textValue <- implode(c(textValue,lines[allLines]), sep=" ")
            }
            
            textValue <- gsub("[\\[\\]]", "", textValue, perl=TRUE)
            pieces <- unlist(strsplit(textValue, ",", fixed=TRUE))
            
            value <- NULL
            endPiece <- 0
            for (i in seq_along(pieces))
            {
                if (i <= endPiece)
                    next
                else if (pieces[i] %~% "^\\s*\"")
                {
                    endQuotePieces <- grep("\"\\s*$", pieces[i:length(pieces)], perl=TRUE)
                    if (length(endQuotePieces) == 0)
                        report(OL$Error, "YAML syntax error: unmatched quotation mark")
                    
                    endPiece <- endQuotePieces[1] + i - 1
                    compositePiece <- implode(pieces[i:endPiece], sep=",")
                    value <- c(value, trimWhitespaceAndQuotes(compositePiece))
                }
                else
                    value <- c(value, trimWhitespaceAndQuotes(pieces[i]))
            }
            
            lines <- lines[-(1:endLine)]
        }
        else
        {
            value <- trimWhitespaceAndQuotes(textValue)
            lines <- lines[-1]
        }
        
        mapping[[key]] <- value
    }
    
    if (length(unlabelled) > 0)
        mapping[[".unlabelled"]] <- unlabelled
    
    return (mapping)
}

writeYaml <- function (mapping, fileName, capitaliseLabels = TRUE)
{
    if (!is.list(mapping))
        return (invisible(NULL))
    
    lines <- character(0)
    for (i in seq_along(mapping))
    {
        if (capitaliseLabels)
            label <- sub("^([a-z])", "\\U\\1", names(mapping)[i], perl=TRUE)
        else
            label <- names(mapping)[i]
        
        if (length(mapping[[i]]) > 1)
            value <- paste("[", implode(mapping[[i]],", "), "]")
        else
            value <- mapping[[i]]
        line <- paste(label, value, sep=": ")
        lines <- c(lines, line)
    }
    
    writeLines(lines, fileName)
}
