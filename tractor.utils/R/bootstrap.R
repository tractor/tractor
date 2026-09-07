#' Set up and run a TractoR experiment script
#'
#' This function performs the setup required to run a TractoR "experiment"
#' script from R: it attaches the core packages, sets standard options,
#' parses command-line-style arguments and configuration variables, sources
#' the script, and then calls its `runExperiment()` function. It underlies
#' the `tractor` shell command, but can also be used directly from an R
#' session (see [callExperiment()] and [debugExperiment()] for convenient
#' wrappers).
#'
#' @param scriptFile A string giving the path to the experiment script.
#' @param workingDirectory A string giving the working directory to change
#'   to before running the experiment. Defaults to the current directory.
#' @param outputLevel An integer giving the `reportr` output level to use,
#'   typically one of the constants from `OL`.
#' @param configFiles A character vector of paths to YAML configuration
#'   files (see [readYaml()]), or `NULL`.
#' @param configText A single string containing whitespace-separated
#'   arguments and `name:value` configuration pairs, as would typically be
#'   assembled from the command line.
#' @param parallelisationFactor An integer giving the number of cores to
#'   parallelise over, if the `parallel` package is available. Values of
#'   one or less disable parallelisation.
#' @param profile Boolean value: if `TRUE`, the experiment is run under
#'   `Rprof()` profiling, with results written to `tractor-Rprof.out` in
#'   the working directory.
#' @param standalone Boolean value. If `TRUE`, the default, the R session
#'   will quit once the experiment completes (or fails). If `FALSE`,
#'   control returns to the caller.
#' @param debug Boolean value: if `TRUE`, `runExperiment()` is run under
#'   the R debugger.
#' @param breakpoint A string identifying a location within `scriptFile` at
#'   which to set a breakpoint (see `setBreakpoint()`), or `NULL` for none.
#' @return Called for its side effect of running the experiment script. If
#'   `scriptFile` does not define a `runExperiment()` function, `NULL` is
#'   returned invisibly without further action.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
bootstrapExperiment <- function (scriptFile, workingDirectory = getwd(), outputLevel = OL$Warning, configFiles = NULL, configText = NULL, parallelisationFactor = 1, profile = FALSE, standalone = TRUE, debug = FALSE, breakpoint = NULL)
{
    profile <- as.logical(profile)
    
    if (standalone)
        on.exit(quit(save="no"))
    
    for (packageName in c("utils","grDevices","graphics","stats","methods","ore","reportr","tractor.base","tractor.utils","tractor.session"))
        library(packageName, character.only=TRUE)
    
    if (capabilities("aqua"))
        options(device="quartz")
    
    if (Sys.getenv("COLUMNS") != "")
        options(width=as.integer(Sys.getenv("COLUMNS")))
    
    # Get the canonical path for TRACTOR_HOME, otherwise changing the working directory may break things
    Sys.setenv(TRACTOR_HOME=expandFileName(Sys.getenv("TRACTOR_HOME")))
    
    setOutputLevel(outputLevel)
    options(reportrStackTraceLevel=OL$Warning)
    
    if (isValidAs(parallelisationFactor,"integer") && as.integer(parallelisationFactor) > 1)
    {
        if (system.file(package="parallel") != "")
        {
            library(parallel)
            options(mc.cores=as.integer(parallelisationFactor))
        }
        else
            report(OL$Warning, "The \"parallel\" package is not installed - code will not be parallelised")
    }
    
    withReportrHandlers({
        source(scriptFile)
        
        if (!is.null(breakpoint))
            setBreakpoint(scriptFile, breakpoint, nameonly=FALSE)
        
        config <- list()
        textFragments <- ore.split("\\s+", configText)
        labelled <- textFragments %~% "^\\s*(\\w+):(.+?)\\s*$"
        match <- ore.lastmatch(FALSE)
        for (i in which(labelled))
            config[[match[i,,1]]] <- match[i,,2]
            
        assign("Arguments", textFragments[!labelled], envir=globalenv())
        assign("ConfigVariables", deduplicate(config,readYaml(configFiles)), envir=globalenv())
        
        setwd(workingDirectory)
        
        if (!exists("runExperiment"))
        {
            on.exit(NULL)
            return (invisible(NULL))
        }
        else
        {
            if (debug)
                debug(runExperiment)
            
            if (profile)
                Rprof("tractor-Rprof.out")
            
            runExperiment()
            
            if (profile)
                Rprof(NULL)
        }
    })
    
    reportFlags()
    
    if (any(names(ConfigVariables) %in% names(config)))
    {
        unusedNames <- paste0("\"", names(ConfigVariables)[names(ConfigVariables) %in% names(config)], "\"")
        if (length(unusedNames) == 1)
            report(OL$Warning, "Configuration variable #{unusedNames} was not used")
        else
            report(OL$Warning, "Configuration variables #{implode(unusedNames,sep=', ',finalSep=' and ')} were not used")
    }
}

#' Print usage information for an experiment script
#'
#' This function prints a usage summary for a TractoR experiment script,
#' listing the configuration variables it accepts (as determined by
#' scanning the script's source for calls to [getConfigVariable()]), along
#' with any arguments, examples or description given via special `#@@args`,
#' `#@@example` and `#@@desc` comments.
#'
#' @param scriptFile A string giving the path to the experiment script.
#' @param fill Boolean value: if `TRUE`, the output is wrapped to fit the
#'   width of the terminal (see `cat()`'s `fill` argument); otherwise each
#'   line is printed as-is.
#' @return Called for its side effect of printing usage information. `NULL`
#'   is returned invisibly.
#' @author Jon Clayden
#' @export
describeExperiment <- function (scriptFile, fill = FALSE)
{
    inputLines <- readLines(scriptFile)
    outputLines <- es("OPTIONS for script #{scriptFile} (* required)")
    
    getConfigVariable <- function (name, defaultValue = NULL, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL, deprecated = FALSE, multiple = FALSE)
    {
        # Don't show deprecated config variables
        if (!deprecated)
        {
            leadString <- ifelse(errorIfMissing, " * ", "   ")
            if (identical(defaultValue, TRUE))
                defaultValueString <- "true"
            else if (identical(defaultValue, FALSE))
                defaultValueString <- "false"
            else
                defaultValueString <- ifelse(is.null(defaultValue), "(no value)", as.character(defaultValue))
            
            if (!is.null(validValues))
            {
                otherValues <- (if (is.null(defaultValue)) validValues else validValues[-match(defaultValue,validValues)])
                defaultValueString <- paste(defaultValueString, " [", implode(otherValues,sep=","), "]", sep="")
            }
            outputLines <<- c(outputLines, paste(leadString, name, ": ", defaultValueString, sep=""))
        }
    }
    
    relevantInputLines <- inputLines %~|% ore("getConfigVariable",syntax="fixed")
    for (currentLine in relevantInputLines)
        eval(parse(text=currentLine))
    
    if (length(outputLines) == 1)
        outputLines <- c(outputLines, "   None")
    
    if (any(inputLines %~% "^\\s*\\#\\@args\\s+(.+)$"))
        outputLines <- c(outputLines, es("ARGUMENTS: #{implode(groups(ore.lastmatch()),sep=', ')}"))
    if (any(inputLines %~% "^\\s*\\#\\@example\\s+(.+)$"))
        outputLines <- c(outputLines, "\nEXAMPLES:", groups(ore.lastmatch()))
    if (any(inputLines %~% "^\\s*\\#\\@desc\\s+(.+)$"))
        outputLines <- c(outputLines, "\nDESCRIPTION:", implode(groups(ore.lastmatch()),sep=" "))
    
    if (fill == FALSE)
        cat(outputLines, sep="\n")
    else
        lapply(strsplit(outputLines," ",fixed=TRUE), cat, fill=fill)
    
    invisible(NULL)
}

#' Standard search paths for experiment scripts
#'
#' This function returns the ordered list of directories that are searched
#' for TractoR experiment scripts, by [findExperiment()] and
#' [scanExperiments()]. In order, these are the current working directory,
#' the user's `~/.tractor` directory, any directories listed in the
#' `TRACTOR_PATH` environment variable, the `tractor/experiments`
#' directories of any packages named in the `TRACTOR_PACKAGES` environment
#' variable, and finally the standard experiment directory shipped with
#' TractoR.
#'
#' @return A character vector of directory paths.
#' @author Jon Clayden
#' @export
experimentPaths <- function ()
{
    packagePaths <- unlist(lapply(splitAndConvertString(Sys.getenv("TRACTOR_PACKAGES"), "[:,]"), function(p) system.file("tractor", "experiments", package=p)))
    return (c(getwd(),
              file.path(Sys.getenv("HOME"), ".tractor"),
              splitAndConvertString(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE),
              packagePaths[packagePaths != ""],
              file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "experiments")))
}

#' Find and summarise available experiment scripts
#'
#' This function scans the standard experiment script search paths (see
#' [experimentPaths()]) for R scripts, and extracts metadata about each one
#' from special `#@group`, `#@args`, `#@desc`, `#@interactive`,
#' `#@nohistory` and `#@example` comments found in its source.
#'
#' @param pattern An optional regular expression used to filter scripts by
#'   name. If `NULL`, the default, all scripts found are returned.
#' @return A `data.frame` with one row per matching script and columns
#'   `name`, `group`, `path`, `args`, `description`, `interactive`,
#'   `nohistory` and `example`.
#' @author Jon Clayden
#' @export
scanExperiments <- function (pattern = NULL)
{
    path <- list.files(experimentPaths(), "\\.[rR]$", full.names=TRUE)
    name <- ensureFileSuffix(basename(path), NULL, strip="[rR]")
    
    if (!is.null(pattern))
    {
        match <- name %~% pattern
        path <- path[match]
        name <- name[match]
    }
    
    n <- length(path)
    interactive <- nohistory <- example <- logical(n)
    args <- description <- group <- character(n)
    for (i in seq_len(n))
    {
        lines <- readLines(path[i])
        interactive[i] <- any(lines %~% "^\\s*\\#\\@interactive\\s+TRUE")
        nohistory[i] <- any(lines %~% "^\\s*\\#\\@nohistory\\s+TRUE")
        example[i] <- any(lines %~% "^\\s*\\#\\@example\\s+(.+)$")
        
        if (any(lines %~% "^\\s*\\#\\@args\\s+(.+)$"))
            args[i] <- implode(groups(ore.lastmatch()), sep=", ")
        if (any(lines %~% "^\\s*\\#\\@desc\\s+(.+)$"))
            description[i] <- implode(groups(ore.lastmatch()), sep=" ")
        if (any(lines %~% "^\\s*\\#\\@group\\s+(.+)$"))
            group[i] <- implode(groups(ore.lastmatch()), sep=" ")
    }
    
    return (data.frame(name=name, group=group, path=path, args=args, description=description, interactive=interactive, nohistory=nohistory, example=example))
}

#' Locate an experiment script by name
#'
#' This function searches the standard experiment script search paths (see
#' [experimentPaths()]) for a script with the given name, and returns the
#' path to the first match. An error is raised if no matching script can be
#' found.
#'
#' @param exptName A string giving the name of the experiment, without the
#'   `.R` file extension.
#' @return A string giving the path to the matching experiment script.
#' @author Jon Clayden
#' @export
findExperiment <- function (exptName)
{
    exptFile <- ensureFileSuffix(exptName, "R")
    possibleLocations <- file.path(experimentPaths(), exptFile)
    filesExist <- file.exists(possibleLocations)
    
    if (sum(filesExist) == 0)
        report(OL$Error, "Experiment script \"", exptFile, "\" not found")
    else
    {
        realLocations <- possibleLocations[filesExist]
        return (realLocations[1])
    }
}

#' Run a named experiment script from within R
#'
#' This function is a convenient way to run a TractoR experiment script
#' from an interactive R session (as opposed to via the `tractor` shell
#' command). It locates the script using [findExperiment()] and then runs
#' it via [bootstrapExperiment()], with `standalone` set to `FALSE` so that
#' control returns to the caller.
#'
#' @param exptName A string giving the name of the experiment. If it
#'   contains whitespace and `args` is `NULL`, it is assumed to bundle the
#'   experiment name and its arguments together, as on the command line,
#'   and will be split accordingly.
#' @param args A character vector of arguments to the experiment, or a
#'   single string containing all of them, or `NULL`.
#' @param configFiles A character vector of paths to YAML configuration
#'   files, or `NULL`.
#' @param outputLevel An integer giving the `reportr` output level to use.
#'   Defaults to the level currently in effect.
#' @param ... Additional arguments to [bootstrapExperiment()].
#' @return Called for its side effect of running the experiment script.
#' @author Jon Clayden
#' @export
callExperiment <- function (exptName, args = NULL, configFiles = NULL, outputLevel = getOutputLevel(), ...)
{
    if (length(exptName) != 1L)
        report(OL$Error, "Experiment name should be a single string")
    
    # If the experiment name contains a space and the arguments are empty,
    # assume they're bundled into one string command-line style
    if (exptName %~% "\\s" && is.null(args))
    {
        match <- ore.search("^(\\S+)\\s+(.+)$", exptName)
        exptName <- match[1,1]
        args <- match[1,2]
    }
    
    scriptFile <- findExperiment(exptName)
    bootstrapExperiment(scriptFile, outputLevel=outputLevel, configFiles=configFiles, configText=implode(args,sep=" "), standalone=FALSE, ...)
    
    # Clean up global variables created by bootstrapExperiment()
    rm(list=c("Arguments","ConfigVariables"), envir=globalenv())
}

#' Run a named experiment script under the debugger
#'
#' This function is analogous to [callExperiment()], but runs the located
#' experiment script at `OL$Debug` output level and under the R debugger,
#' either stepping into `runExperiment()` directly, or setting a breakpoint
#' at a particular line if one is given.
#'
#' @param exptName A string giving the name of the experiment. If it
#'   contains whitespace and `args` is `NULL`, it is assumed to bundle the
#'   experiment name and its arguments together, as on the command line,
#'   and will be split accordingly.
#' @param args A character vector of arguments to the experiment, or a
#'   single string containing all of them, or `NULL`.
#' @param configFiles A character vector of paths to YAML configuration
#'   files, or `NULL`.
#' @param breakpoint A string identifying a location within the experiment
#'   script at which to set a breakpoint (see `setBreakpoint()`). If
#'   `NULL`, the default, `runExperiment()` itself is stepped into instead.
#' @param ... Additional arguments to [bootstrapExperiment()].
#' @return Called for its side effect of running the experiment script
#'   under the debugger.
#' @author Jon Clayden
#' @export
debugExperiment <- function (exptName, args = NULL, configFiles = NULL, breakpoint = NULL, ...)
{
    if (length(exptName) != 1L)
        report(OL$Error, "Experiment name should be a single string")
    
    if (exptName %~% "\\s" && is.null(args))
    {
        match <- ore.search("^(\\S+)\\s+(.+)$", exptName)
        exptName <- match[1,1]
        args <- match[1,2]
    }
    
    scriptFile <- findExperiment(exptName)
    report(OL$Info, "Debugging experiment script ", scriptFile)
    bootstrapExperiment(scriptFile, outputLevel=OL$Debug, configFiles=configFiles, configText=implode(args,sep=" "), standalone=FALSE, debug=is.null(breakpoint), breakpoint=breakpoint, ...)
}
