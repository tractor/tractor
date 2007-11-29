createWorkspaceFromYaml <- function (fileName = NULL, text = NULL, environment = .GlobalEnv)
{
    .asAppropriateType <- function (x)
    {
        if (x %~% "^\\s*$")
            return (NULL)
        else if (is.na(suppressWarnings(as.numeric(x))))
        {
            text <- as.character(x)
            text <- sub("^\\s*\"?", "", text, perl=TRUE)
            text <- sub("\"?\\s*$", "", text, perl=TRUE)
            return (text)
        }
        else
            return (as.numeric(x))
    }
    
    if (!is.null(fileName) && !file.exists(fileName))
        output(OL$Error, "Configuration file ", fileName, " does not exist")
    
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
        output(OL$Error, "File name or YAML text must be specified")
    
    arguments <- character(0)
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
        
        # Everything before the first colon in the line is the key; the rest
        # is the associated value
        key <- sub("\\s*:\\s*.*$", "", lines[1], perl=TRUE)
        key <- sub("^\\s+", "", key, perl=TRUE)
        textValue <- sub("^\\s*\\w+\\s*:\\s*", "", lines[1], perl=TRUE)
        
        if (key == textValue)
        {
            if (usingFile)
                output(OL$Warning, "A line in the configuration file ", fileName, " has no key")
            else
                arguments <- c(arguments, textValue)
            
            lines <- lines[-1]
            next
        }
        output(OL$Debug, "Key is ", key, "; value is ", textValue)

        # Values starting with a square bracket are sequences (vectors in R)
        if (regexpr("[", textValue, fixed=TRUE) == 1)
        {
            endBracketLines <- setdiff(grep("]", lines, fixed=TRUE), grep("^\\s*#", lines, perl=TRUE))
            if (length(endBracketLines) == 0)
                output(OL$Error, "Configuration file syntax error: unmatched bracket")
            
            endLine <- endBracketLines[1]
            if (endLine > 1)
            {
                allLines <- setdiff(2:endLine, grep("^\\s*#", lines, perl=TRUE))
                textValue <- paste(c(textValue,lines[allLines]), collapse=" ")
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
                        output(OL$Error, "Configuration file syntax error: unmatched quotation mark")
                    
                    endPiece <- endQuotePieces[1] + i - 1
                    compositePiece <- paste(pieces[i:endPiece], collapse=",")
                    value <- c(value, .asAppropriateType(compositePiece))
                }
                else
                    value <- c(value, .asAppropriateType(pieces[i]))
            }
            
            lines <- lines[-(1:endLine)]
        }
        else
        {
            value <- .asAppropriateType(textValue)
            lines <- lines[-1]
        }
        
        assign(key, value, pos=environment)
    }
    
    if (length(arguments) > 0)
        assign("Arguments", arguments, pos=environment)
}

writeReportToYaml <- function (results, fileName = "tractor_report.yaml")
{
    if (!is.list(results))
        return (invisible(NULL))
    
    lines <- character(0)
    for (i in seq_along(results))
    {
        label <- sub("^([a-z])", "\\U\\1", names(results)[i], perl=TRUE)
        if (length(results[[i]]) > 1)
            value <- paste("[", implode(results[[i]],", "), "]")
        else
            value <- results[[i]]
        line <- paste(label, value, sep=": ")
        lines <- c(lines, line)
    }
    
    writeLines(lines, fileName)
}
