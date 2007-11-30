setOutputLevel <- function (level)
{
    if (level %in% OL$Debug:OL$Error)
        options(tractorOutputLevel=level)
}

output <- function (level, ..., default = NULL)
{
    if (is.null(getOption("tractorOutputLevel")))
    {
        cat("INFO: Output level is not set; defaulting to \"Info\"\n")
        options(tractorOutputLevel=OL$Info)
    }
    
    outputLevel <- getOption("tractorOutputLevel")
    prefixStrings <- c("DEBUG: ", "VERBOSE: ", "INFO: ", "WARNING: ")

    nStars <- length(sys.calls()) - length(grep("trycatch", sys.calls(), ignore.case=TRUE)) - 1
    leadingSpace <- implode(rep("* ", nStars))
    if ((level < OL$Question) && (outputLevel <= level))
        cat(paste(leadingSpace, prefixStrings[level], ..., "\n", sep=""))
    
    else if (level == OL$Question)
    {
        if (outputLevel == OL$Error)
            return (default)
        else
        {
            cat(paste(leadingSpace, "QUESTION: ", ..., "\n", sep=""))
            ans <- scan(what=character(0), nlines=1, quiet=TRUE)
            return (ans)
        }
    }
    else if (level == OL$Error)
    {
        if (outputLevel == OL$Debug)
        {
            cat("ERROR - stack trace follows\n")
            calls <- sys.calls()
            for (i in 1:length(calls))
            {
                cat(rep("* ", i), sep="")
                print(calls[[i]])
            }
        }
        stop(..., call.=FALSE)
    }
}

"%~%" <- function (X, Y)
{
    if (!is.character(X) || !is.character(Y) || length(Y) != 1)
        output(OL$Error, "Parameters for matching on a regular expression are not valid")
    
    matchLocs <- regexpr(Y, X, perl=TRUE)
    return (!(matchLocs == -1))
}

implode <- function (strings, sep = "")
{
    strings <- as.character(strings)
    
    if (length(strings) == 1)
        return (strings[1])
    else if (length(strings) > 1)
    {
        result <- strings[1]
        for (i in 2:length(strings))
            result <- paste(result, strings[i], sep=sep)    
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
    fileName <- gsub("/\\.(?!\\.)", "", fileName, perl=TRUE)
    while (length(grep('..', fileName, fixed=TRUE) > 0))
        fileName <- sub("/[^/]*[^./][^/]*/\\.\\.(?!\\.)", "", fileName, perl=TRUE)
    fileName <- gsub("/*$", "", fileName, perl=TRUE)
    
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

getWithDefault <- function (name, defaultValue = NULL, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE)
{
    if (is.null(mode) && !is.null(defaultValue))
        mode <- mode(defaultValue)
    
    if (!exists(name))
    {
        if (errorIfMissing)
            output(OL$Error, "The configuration variable \"", name, "\" must be specified")
        else
            return (defaultValue)
    }
    else if (is.null(mode) || mode == "NULL")
        return (get(name))
    else
    {
        value <- get(paste("as", mode, sep="."))(get(name))
        if (is.na(value))
        {
            if (errorIfInvalid)
                output(OL$Error, "The configuration variable \"", name, "\" does not have a suitable value")
            else
                return (defaultValue)
        }
        else
            return (value)
    }
}

nArguments <- function ()
{
    if (!exists("Arguments"))
        return (0)
    else
        return (length(get("Arguments")))
}

requireArguments <- function (minLength = 1, errorIfMissing = TRUE)
{
    if (nArguments() < minLength)
    {
        if (errorIfMissing)
            output(OL$Error, "At least ", minLength, " argument(s) must be specified")
        else
            invisible (FALSE)
    }
    else
        invisible (TRUE)
}

producesError <- function (expr, silent = TRUE)
{
    returnValue <- try(expr, silent)
    return (class(returnValue) == "try-error")
}
