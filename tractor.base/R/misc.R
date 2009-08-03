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

rawToCharQuiet <- function (...)
{
   suppressWarnings(rawToChar(...))
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

insertRowAt <- function (index, x, rowData)
{
    x <- promote(x)
    end <- nrow(x)
    row <- matrix(rowData, nrow=1)
    
    if (index == 1)
        result <- rbind(row, x)
    else if (index == end+1)
        result <- rbind(x, row)
    else if (index > 1 && index <= end)
        result <- rbind(x[1:(index-1),,drop=FALSE], row, x[index:end,,drop=FALSE])
    else
        output(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

insertColumnAt <- function (index, x, colData)
{
    x <- promote(x, byrow=TRUE)
    end <- ncol(x)
    col <- matrix(colData, ncol=1)
    
    if (index == 1)
        result <- cbind(col, x)
    else if (index == end+1)
        result <- cbind(x, col)
    else if (index > 1 && index <= end)
        result <- cbind(x[,1:(index-1),drop=FALSE], col, x[,index:end,drop=FALSE])
    else
        output(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

restrict <- function (x, fun = NULL, ..., invert = FALSE, na.rm=TRUE)
{
    if (!is.vector(x))
        output(OL$Error, "The \"restrict\" function only works for vectors")
    if (is.null(fun))
        matches <- rep(TRUE, length(x))
    else
    {
        fun <- match.fun(fun)
        matches <- fun(x, ...)
        if (!is.logical(matches))
            output(OL$Error, "The result of applying the specified function to the vector is not of logical type")
    }
    
    if (invert)
        matches <- !matches
    if (na.rm)
        matches <- matches & !is.na(x)
    
    return (x[matches])
}

equivalent <- function (x, y, signMatters = TRUE, ...)
{
    if (signMatters)
        return (isTRUE(all.equal(x, y, ...)))
    else
        return (isTRUE(all.equal(abs(x), abs(y), ...)))
}

isValidAs <- function (value, mode)
{
    coercedValue <- suppressWarnings(as(value, mode))
    return (!any(is.na(coercedValue)))
}

splitAndConvertString <- function (string, split = "", mode = "character", errorIfInvalid = FALSE, ...)
{
    values <- unlist(strsplit(string, split, ...))
    values <- suppressWarnings(as(values, mode))
    
    if (errorIfInvalid && any(is.na(values)))
        output(OL$Error, "Specified list, \"", implode(string,sep=" "), "\", is not valid here")
    else
        return (values)
}

getWithDefault <- function (name, defaultValue, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL)
{
    reportInvalid <- function ()
    {
        level <- ifelse(errorIfInvalid, OL$Error, OL$Warning)
        message <- paste("The configuration variable \"", name, "\" does not have a suitable value", ifelse(errorIfInvalid,""," - using default"), sep="")
        output(level, message)
    }
    
    matchAgainstValidValues <- function (currentValue)
    {
        if (is.null(validValues))
            return (currentValue)
        else if (!is.null(mode) && mode == "character")
        {
            currentValue <- tolower(currentValue)
            currentValidValues <- tolower(validValues)
        }
        else
            currentValidValues <- validValues
        
        loc <- match(currentValue, currentValidValues, nomatch=0)
        if (loc != 0)
            return (validValues[loc])
        else
        {
            reportInvalid()
            return (defaultValue)
        }
    }
    
    if (is.null(mode) && !is.null(defaultValue))
        mode <- mode(defaultValue)
    
    if (!exists(name))
    {
        if (errorIfMissing)
            output(OL$Error, "The configuration variable \"", name, "\" must be specified")
        else
            return (defaultValue)
    }
    else
    {
        value <- get(name)
        if (is.null(mode) || mode == "NULL")
            return (matchAgainstValidValues(value))
        else if (!isValidAs(value, mode))
        {
            reportInvalid()
            return (defaultValue)
        }
        else
        {
            value <- as(value, mode)
            return (matchAgainstValidValues(value))
        }
    }
}

nArguments <- function ()
{
    if (!exists("Arguments"))
        return (0)
    else
        return (length(get("Arguments")))
}

requireArguments <- function (...)
{
    args <- c(...)
    
    if (is.numeric(args) && nArguments() < args)
        output(OL$Error, "At least ", args, " argument(s) must be specified")
    else if (is.character(args) && nArguments() < length(args))
        output(OL$Error, "At least ", length(args), " argument(s) must be specified: ", implode(args,", "))
}

producesError <- function (expr, silent = TRUE)
{
    returnValue <- try(expr, silent)
    return ("try-error" %in% class(returnValue))
}
