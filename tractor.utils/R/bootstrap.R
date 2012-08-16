bootstrapExperiment <- function (scriptFile, workingDirectory, reportFile, outputLevel = OL$Warning, configFiles = "/dev/null", configText = "", parallelisationFactor = 1, debug = FALSE)
{
    if (!debug)
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
    
    if (is.list(results))
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
                defaultValueString <- paste(defaultValueString, " [", paste(otherValues,collapse=","), "]", sep="")
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
        argsString <- paste(sub("^\\s*\\#\\@args\\s*", "", relevantInputLines, perl=TRUE), collapse=", ")
        outputLines <- c(outputLines, paste("ARGUMENTS:", argsString, sep=" "))
    }
    
    relevantInputLines <- grep("#@desc", inputLines, value=TRUE, fixed=TRUE)
    if (length(relevantInputLines) != 0)
    {
        descriptionString <- paste(sub("^\\s*\\#\\@desc\\s*", "", relevantInputLines, perl=TRUE), collapse=" ")
        outputLines <- c(outputLines, "", descriptionString)
    }
    
    if (fill == FALSE)
        cat(paste(outputLines, "\n"), collapse="\n")
    else
        lapply(strsplit(outputLines," ",fixed=TRUE), cat, fill=fill)
    
    invisible(NULL)
}

debugExperiment <- function (exptName, args = "", configFiles = "/dev/null")
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
        report(OL$Info, "Debugging experiment script ", realLocations[1])
        bootstrapExperiment(realLocations[1], ".", "/dev/null", OL$Debug, configFiles, args, debug=TRUE)
    }
}
