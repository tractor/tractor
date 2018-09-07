splitAndConvertString <- function (string, split = "", mode = "character", errorIfInvalid = FALSE, allowRanges = TRUE, ...)
{
    values <- unlist(strsplit(string, split, ...))
    if (allowRanges && mode=="integer")
    {
        values <- unlist(lapply(values, function (x) {
            x <- sub("(\\d)-", "\\1:", x, perl=TRUE)
            eval(parse(text=x))
        }))
    }
    else
        values <- suppressWarnings(as(values, mode))
    
    if (errorIfInvalid && any(is.na(values)))
        report(OL$Error, "Specified list, \"", implode(string,sep=" "), "\", is not valid here")
    else
        return (values)
}

# Custom character-to-logical coercion to allow for more TRUE and FALSE values
setAs("character", "logical", function(from) {
    result <- rep(NA, length(from))
    result[tolower(from) %in% c("t","true","1","yes","yup","yep","yeah","hellyeah")] <- TRUE
    result[tolower(from) %in% c("f","false","0","no","nope","hellno")] <- FALSE
    return (result)
})

isValidAs <- function (value, mode)
{
    coercedValue <- suppressWarnings(as(value, mode))
    return (!any(is.na(coercedValue)))
}

getConfigVariable <- function (name, defaultValue = NULL, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL, deprecated = FALSE)
{
    reportInvalid <- function ()
    {
        level <- ifelse(errorIfInvalid, OL$Error, OL$Warning)
        message <- paste("The configuration variable \"", name, "\" does not have a suitable and unambiguous value", ifelse(errorIfInvalid,""," - using default"), sep="")
        report(level, message)
    }
    
    matchAgainstValidValues <- function (currentValue)
    {
        if (is.null(validValues))
            return (currentValue)
        else if (isTRUE(mode == "character"))
            loc <- pmatch(tolower(currentValue), tolower(validValues), nomatch=0)
        else
            loc <- match(currentValue, validValues, nomatch=0)
        
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
    
    if (!exists("ConfigVariables") || !any(tolower(name) == tolower(names(ConfigVariables))))
    {
        if (errorIfMissing)
            report(OL$Error, "The configuration variable \"#{name}\" must be specified")
        else
            return (defaultValue)
    }
    else
    {
        if (deprecated)
            report(OL$Warning, "The configuration variable \"#{name}\" is deprecated")
        
        loc <- which(tolower(name) == tolower(names(ConfigVariables)))
        value <- ConfigVariables[[loc]]
        ConfigVariables[[loc]] <<- NULL
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
        report(OL$Error, "At least ", args, " argument(s) must be specified")
    else if (is.character(args) && nArguments() < length(args))
        report(OL$Error, "At least ", length(args), " argument(s) must be specified: ", implode(args,", "))
}

expandArguments <- function (arguments, workingDirectory = getwd())
{
    setwd(workingDirectory)
    arguments <- ore.split("\\s+", arguments)
    for (i in seq_along(arguments))
    {
        fileName <- identifyImageFileNames(arguments[i], errorIfMissing=FALSE)
        if (!is.null(fileName))
            arguments[i] <- fileName$imageFile
    }
    cat(implode(arguments, sep=" "))
}
