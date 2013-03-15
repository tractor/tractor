bootstrapExperiment <- function (scriptFile, workingDirectory = getwd(), reportFile = NULL, outputLevel = OL$Warning, configFiles = NULL, configText = NULL, parallelisationFactor = 1, standalone = TRUE, debug = FALSE)
{
    if (standalone)
        on.exit(quit(save="no"))
    
    for (packageName in c("utils","grDevices","graphics","stats","methods","reportr","tractor.base"))
        library(packageName, character.only=TRUE)
    
    if (capabilities()["aqua"])
        options(device="quartz")
    
    setOutputLevel(outputLevel)
    options(reportrStackTraceLevel=OL$Warning)
    
    if (isValidAs(parallelisationFactor,"integer") && as.integer(parallelisationFactor) > 1)
    {
        if (system.file(package="parallel") != "")
        {
            library(parallel)
            options(mc.cores=as.integer(parallelisationFactor))
        }
        else if (system.file(package="multicore") != "")
        {
            library(multicore)
            options(cores=as.integer(parallelisationFactor))
        }
        else
            report(OL$Warning, "The \"multicore\" package is not installed - code will not be parallelised")
    }
    
    results <- withReportrHandlers({
        source(scriptFile)
        
        config <- readYaml(configFiles)
        config <- readYaml(text=configText, init=config)
        if (!is.null(config[[".unlabelled"]]))
        {
            assign("Arguments", config[[".unlabelled"]], envir=globalenv())
            config[[".unlabelled"]] <- NULL
        }
        assign("ConfigVariables", config, envir=globalenv())
        
        setwd(workingDirectory)
        
        if (!exists("runExperiment"))
            report(OL$Error, "The experiment script does not contain a \"runExperiment\" function")
        
        if (debug)
            debug(runExperiment)
        
        runExperiment()
    })
    
    reportFlags()
    
    if (!standalone)
        return (results)
    else if (!is.null(reportFile) && is.list(results))
        writeYaml(results, fileName=reportFile)
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
            defaultValueString <- ifelse(is.null(defaultValue), "NULL", as.character(defaultValue))
            if (!is.null(validValues))
            {
                otherValues <- (if (is.null(defaultValue)) validValues else validValues[-match(defaultValue,validValues)])
                defaultValueString <- paste(defaultValueString, " [", implode(otherValues,sep=","), "]", sep="")
            }
            outputLines <<- c(outputLines, paste(leadString, name, ": ", defaultValueString, sep=""))
        }
    }
    
    relevantInputLines <- grep("getConfigVariable", inputLines, value=TRUE, fixed=TRUE)
    for (currentLine in relevantInputLines)
        eval(parse(text=currentLine))
    
    if (length(outputLines) == 1)
        outputLines <- c(outputLines, "   None")
    
    relevantInputLines <- grep("#@args", inputLines, value=TRUE, fixed=TRUE)
    if (length(relevantInputLines) != 0)
    {
        argsString <- implode(sub("^\\s*\\#\\@args\\s*", "", relevantInputLines, perl=TRUE), sep=", ")
        outputLines <- c(outputLines, paste("ARGUMENTS:", argsString, sep=" "))
    }
    
    relevantInputLines <- grep("#@desc", inputLines, value=TRUE, fixed=TRUE)
    if (length(relevantInputLines) != 0)
    {
        descriptionString <- implode(sub("^\\s*\\#\\@desc\\s*", "", relevantInputLines, perl=TRUE), sep=" ")
        outputLines <- c(outputLines, "", descriptionString)
    }
    
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
                  file.path(Sys.getenv("TRACTOR_HOME"), "share", "experiments"))
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

debugExperiment <- function (exptName, args = NULL, configFiles = NULL, ...)
{
    scriptFile <- findExperiment(exptName)
    report(OL$Info, "Debugging experiment script ", scriptFile)
    bootstrapExperiment(scriptFile, outputLevel=OL$Debug, configFiles=configFiles, configText=implode(args,sep=" "), standalone=FALSE, debug=TRUE, ...)
}
