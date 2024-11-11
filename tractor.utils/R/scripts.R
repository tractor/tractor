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

# Custom character-to-integer coercion to handle ranges
setAs("character", "integer", function(from) {
    result <- unlist(lapply(from, function(x) {
        match <- ore.search("^(\\d+)[:-](\\d+)$", x)
        if (!is.null(match))
            seq.int(from=as.integer(match[,1]), to=as.integer(match[,2]))
        else
            suppressWarnings(as.integer(x))
    }))
    return (result)
})

isValidAs <- function (value, mode)
{
    coercedValue <- suppressWarnings(as(value, mode))
    return (!any(is.na(coercedValue)))
}

getConfigVariable <- function (name, defaultValue = NULL, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL, deprecated = FALSE, multiple = FALSE)
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
        else if (multiple && is.character(defaultValue))
            return (as(ore.split(",", defaultValue), mode))
        else
            return (defaultValue)
    }
    else
    {
        if (deprecated)
            report(OL$Warning, "The configuration variable \"#{name}\" is deprecated")
        
        loc <- which(tolower(name) == tolower(names(ConfigVariables)))
        value <- ConfigVariables[[loc]]
        if (multiple)
            value <- ore.split(",", value)
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

expandArguments <- function (arguments, workingDirectory = getwd(), suffixes = TRUE, relative = FALSE)
{
    setOutputLevel(OL$Warning)
    setwd(workingDirectory)
    suffixes <- as.logical(suffixes)
    relative <- as.logical(relative)
    
    arguments <- resolvePath(arguments)
    for (i in seq_along(arguments))
    {
        if (file.exists(arguments[i]))
        {
            arguments[i] <- ifelse(relative, shQuote(relativePath(arguments[i],workingDirectory)), shQuote(arguments[i]))
            next
        }
        fileName <- identifyImageFileNames(arguments[i], errorIfMissing=FALSE)
        if (!is.null(fileName))
            arguments[i] <- ifelse(suffixes, fileName$imageFile, fileName$fileStem)
        if (arguments[i] != names(arguments)[i])
            arguments[i] <- ifelse(relative, shQuote(relativePath(arguments[i],workingDirectory)), shQuote(arguments[i]))
        else if (arguments[i] %~% "=")
        {
            parts <- resolvePath(ore.split("=", arguments[i]))
            for (j in seq_along(parts))
            {
                fileName <- identifyImageFileNames(parts[j], errorIfMissing=FALSE)
                if (!is.null(fileName))
                    parts[j] <- ifelse(suffixes, fileName$imageFile, fileName$fileStem)
                if (parts[j] != names(parts)[j])
                    parts[j] <- ifelse(relative, shQuote(relativePath(parts[j],workingDirectory)), shQuote(parts[j]))
            }
            arguments[i] <- implode(parts, sep="=")
        }
    }
    return (arguments)
}
