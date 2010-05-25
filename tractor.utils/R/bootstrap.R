bootstrapExperiment <- function (scriptFile, workingDirectory, reportFile, outputLevel = OL$Warning, configFiles = "/dev/null", configText = "", parallelisationFactor = 1)
{
    library(utils)
    library(grDevices)
    library(graphics)
    library(stats)
    library(tractor.base)
    
    warningWrapper <- function (warning)
    {
        flag(OL$Warning, warning$message)
        invokeRestart("muffleWarning")
    }
    
    errorWrapper <- function (error)
    {
        output(OL$Error, error$message, toReport=TRUE)
    }
    
    if (isValidAs(parallelisationFactor,"integer") && as.integer(parallelisationFactor) > 1)
    {
        if ("multicore" %in% row.names(installed.packages()))
        {
            library(multicore)
            options(cores=as.integer(parallelisationFactor))
        }
        else
            output(OL$Warning, "The \"multicore\" package is not installed - code will not be parallelised")
    }
    
    options(tractorOutputErrors=TRUE)
    results <- try(withCallingHandlers({
        source(scriptFile)
        setOutputLevel(outputLevel)
        createWorkspaceFromYaml(configFiles)
        createWorkspaceFromYaml(text=configText)
        setwd(workingDirectory)
        
        if (!exists("runExperiment"))
            output(OL$Error, "The experiment script does not contain a \"runExperiment\" function")
        runExperiment()
    }, warning=warningWrapper, error=errorWrapper), silent=TRUE)
    options(tractorOutputErrors=FALSE)
    
    reportFlags()
    writeReportToYaml(results,fileName=reportFile)
}

describeExperiment <- function (scriptFile, fill = FALSE)
{
    inputLines <- readLines(scriptFile)
    outputLines <- paste("OPTIONS for script", scriptFile, "(* required)", sep=" ")
    
    getWithDefault <- function (name, defaultValue, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL)
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
    
    relevantInputLines <- grep("getWithDefault", inputLines, value=TRUE, fixed=TRUE)
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
