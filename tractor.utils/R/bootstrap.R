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

describeExperiment <- function (scriptFile, fill = FALSE)
{
    inputLines <- readLines(scriptFile)
    outputLines <- paste("OPTIONS for script", scriptFile, "(* required)", sep=" ")
    
    getConfigVariable <- function (name, defaultValue = NULL, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL, deprecated = FALSE)
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
                defaultValueString <- ifelse(is.null(defaultValue), "NULL", as.character(defaultValue))
            
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

findExperiment <- function (exptName)
{
    exptFile <- ensureFileSuffix(exptName, "R")    
    pathDirs <- c(".",
                  file.path(Sys.getenv("HOME"), ".tractor"),
                  splitAndConvertString(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE),
                  file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "experiments"))
    possibleLocations <- file.path(pathDirs, exptFile)
    filesExist <- file.exists(possibleLocations)
    
    if (sum(filesExist) == 0)
        report(OL$Error, "Experiment script \"", exptFile, "\" not found")
    else
    {
        realLocations <- possibleLocations[filesExist]
        return (realLocations[1])
    }
}

callExperiment <- function (exptName, args = NULL, configFiles = NULL, outputLevel = getOutputLevel(), ...)
{
    scriptFile <- findExperiment(exptName)
    report(OL$Info, "Running experiment script ", scriptFile)
    bootstrapExperiment(scriptFile, outputLevel=outputLevel, configFiles=configFiles, configText=implode(args,sep=" "), standalone=FALSE, ...)
}

debugExperiment <- function (exptName, args = NULL, configFiles = NULL, breakpoint = NULL, ...)
{
    scriptFile <- findExperiment(exptName)
    report(OL$Info, "Debugging experiment script ", scriptFile)
    bootstrapExperiment(scriptFile, outputLevel=OL$Debug, configFiles=configFiles, configText=implode(args,sep=" "), standalone=FALSE, debug=is.null(breakpoint), breakpoint=breakpoint, ...)
}
