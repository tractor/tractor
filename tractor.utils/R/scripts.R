#' Split a string and convert the pieces to a given mode
#'
#' This function splits a string (or vector of strings) using [strsplit()],
#' and then coerces the resulting pieces to the requested storage mode. It
#' is typically used to parse comma- or colon-separated command-line
#' arguments. Custom `character`-to-`logical` and `character`-to-`integer`
#' coercion methods are registered by this package (via `setAs()`), the
#' latter allowing simple integer ranges such as `"1-3"` to be expanded.
#'
#' @param string A string, or character vector, to split.
#' @param split A string or regular expression to split on, as for
#'   [strsplit()].
#' @param mode A string giving the storage mode to coerce the split pieces
#'   to, such as `"character"`, `"integer"` or `"logical"`.
#' @param errorIfInvalid Boolean value: if `TRUE`, an error is raised if
#'   any piece cannot be coerced to `mode` (i.e., produces `NA`); otherwise
#'   the offending values are silently returned as `NA`.
#' @param allowRanges Boolean value: if `TRUE` and `mode` is `"integer"`,
#'   pieces of the form `"a-b"` are expanded into the corresponding integer
#'   sequence.
#' @param ... Additional arguments to [strsplit()].
#' @return A vector of mode `mode`, containing the converted pieces.
#' @author Jon Clayden
#' @export
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

#' Colour text for terminal output
#'
#' This function wraps strings in ANSI escape codes so that they are shown
#' in colour (and optionally bold, dim or italic) when printed to a
#' capable terminal. If the `TRACTOR_NOCOLOUR` environment variable is set
#' to a true value, the strings are returned unmodified.
#'
#' @param strings A character vector to colour.
#' @param col A string naming the colour to use: one of `"black"`,
#'   `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`, `"cyan"`,
#'   `"white"` or `"default"`.
#' @param mod A string giving a modifier to apply, one of `"none"`,
#'   `"bold"`, `"dim"` or `"italic"`.
#' @return A character vector of the same length as `strings`, with ANSI
#'   escape codes added (unless colour output is disabled).
#' @author Jon Clayden
#' @export
colour <- function (strings, col = "default", mod = c("none","bold","dim","italic"))
{
    if (isTRUE(as(Sys.getenv("TRACTOR_NOCOLOUR"), "logical")))
        return (strings)
    else
    {
        mod <- match.arg(mod)
        modString <- switch(mod, none="", bold="1;", dim="2;", italic="3;")
        code <- switch(tolower(col), black=30L, red=31L, green=32L, yellow=33L, blue=34L, magenta=35L, cyan=36L, white=37L, default=39L)
        return (paste0("\x1b[", modString, code, "m", strings, "\x1b[22;0m"))
    }
}

#' Check whether a value can be validly coerced to a mode
#'
#' This function attempts to coerce `value` to the storage mode `mode`
#' using [as()], and reports whether the result is free of `NA`s (which
#' would indicate a failed conversion).
#'
#' @param value A value, typically a character string, to test.
#' @param mode A string giving the target storage mode, such as
#'   `"integer"` or `"logical"`.
#' @return `TRUE` if `value` can be coerced to `mode` without producing any
#'   `NA` values; `FALSE` otherwise.
#' @author Jon Clayden
#' @export
isValidAs <- function (value, mode)
{
    coercedValue <- suppressWarnings(as(value, mode))
    return (!any(is.na(coercedValue)))
}

#' Retrieve a configuration variable for an experiment script
#'
#' This is the standard way for a TractoR experiment script to retrieve a
#' named configuration variable, as set on the command line or in a YAML
#' configuration file passed to `tractor` (see [bootstrapExperiment()]).
#' It looks the variable up (case-insensitively) in the global
#' `ConfigVariables` list, coerces it to the requested mode, checks it
#' against a set of valid values if given, and falls back to a default if
#' the variable is missing or invalid. Variables are removed from
#' `ConfigVariables` once retrieved, so that [bootstrapExperiment()] can
#' warn about any which were never used.
#'
#' @param name A string giving the name of the configuration variable.
#'   Matching against the names in `ConfigVariables` is case-insensitive.
#' @param defaultValue The value to return if the variable is not set, or
#'   is set to an invalid value. Also used to infer `mode` if the latter is
#'   not given explicitly.
#' @param mode A string giving the storage mode to coerce the variable's
#'   value to, such as `"character"`, `"integer"` or `"logical"`. If
#'   `NULL` and `defaultValue` is given, the mode of `defaultValue` is
#'   used; if `"NULL"`, no coercion or validity checking is performed.
#' @param errorIfMissing Boolean value: if `TRUE`, an error is raised if
#'   the variable is not set, rather than falling back to `defaultValue`.
#' @param errorIfInvalid Boolean value: if `TRUE`, an error is raised if
#'   the variable's value cannot be coerced to `mode`, or does not match
#'   `validValues`, rather than a warning being given and `defaultValue`
#'   used.
#' @param validValues An optional vector of values that the variable is
#'   allowed to take. Partial, case-insensitive matching is used when
#'   `mode` is `"character"`.
#' @param deprecated Boolean value: if `TRUE`, a warning is given if the
#'   variable is set, indicating that it is deprecated.
#' @param multiple Boolean value: if `TRUE`, the variable's value (or
#'   `defaultValue`, if the variable is not set) is split on commas before
#'   being coerced to `mode`, allowing a comma-separated list of values to
#'   be specified.
#' @return The value of the configuration variable, coerced to `mode`; or
#'   `defaultValue` if the variable was not set or was invalid.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
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

#' The number of arguments passed to an experiment script
#'
#' This function returns the number of command-line-style arguments that
#' were passed to the currently running experiment script, as stored in
#' the global `Arguments` vector by [bootstrapExperiment()].
#'
#' @return An integer giving the number of arguments, or zero if no
#'   `Arguments` vector currently exists.
#' @author Jon Clayden
#' @export
nArguments <- function ()
{
    if (!exists("Arguments"))
        return (0)
    else
        return (length(get("Arguments")))
}

#' Require that an experiment script was given sufficient arguments
#'
#' This function checks that the currently running experiment script was
#' passed a sufficient number of command-line-style arguments (see
#' [nArguments()]), raising an error if not. If names are given, the
#' global `Arguments` vector is also named accordingly, so that the
#' expected arguments can subsequently be accessed by name as well as by
#' position.
#'
#' @param ... Either a single integer giving the minimum number of
#'   arguments required, or one or more strings giving names for the
#'   expected arguments (in which case their number also becomes the
#'   minimum required).
#' @param name Boolean value: if `TRUE`, and names were given via `...`,
#'   the global `Arguments` vector is renamed accordingly. This can cause
#'   problems in some contexts, such as when `Arguments` is used with
#'   `apply`-family functions, in which case `FALSE` should be used.
#' @return Called for its side effect of checking (and, optionally, naming)
#'   the arguments. `NULL` is returned invisibly.
#' @author Jon Clayden
#' @export
requireArguments <- function (..., name = TRUE)
{
    args <- c(...)
    
    if (is.numeric(args) && nArguments() < args)
        report(OL$Error, "At least ", args, " argument(s) must be specified")
    else if (is.character(args))
    {
        assert(nArguments() >= length(args), "At least #{length(args)} argument(s) must be specified: ", implode(args,", "))
        
        # Name the global argument vector and reassign it, if required
        # This allows expected arguments to be indexed by name, but can cause problems in some contexts (e.g. for "apply")
        if (name)
        {
            arguments <- get("Arguments")
            names(arguments) <- replace(rep("",length(arguments)), seq_along(args), args)
            assign("Arguments", arguments, envir=globalenv())
        }
    }
}

#' Expand file-based command-line arguments
#'
#' This function resolves command-line-style arguments that look like file
#' or image paths into full paths, quoting them for shell use as
#' appropriate. It is intended for use by scripts (such as `furrow`) that
#' hand arguments on to external programs. Arguments containing an equals
#' sign are treated as `key=value` pairs and each side is expanded
#' separately.
#'
#' @param arguments A character vector of arguments to expand. May be
#'   named, in which case a name differing from its value indicates that
#'   the argument has already been resolved (e.g. by a calling shell
#'   script) and should simply be quoted.
#' @param workingDirectory A string giving the working directory to
#'   resolve relative paths against. The R working directory is changed to
#'   this location for the duration of the call.
#' @param suffixes Boolean value: if `TRUE`, recognised image arguments are
#'   expanded to the full image file name (including extension); if
#'   `FALSE`, the file stem is used instead.
#' @param relative Boolean value: if `TRUE`, expanded paths are made
#'   relative to `workingDirectory` rather than absolute.
#' @return A character vector of the same length as `arguments`, with file
#'   and image paths expanded and shell-quoted as appropriate.
#' @author Jon Clayden
#' @export
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
