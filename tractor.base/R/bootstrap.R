bootstrapExperiment <- function (scriptFile, workingDirectory, reportFile, outputLevel = OL$Warning, configFiles = "/dev/null", configText = "")
{
    library(utils)
    library(graphics)
    library(grDevices)
    library(stats)
    
    warningWrapper <- function (warning)
    {
        flag(OL$Warning, warning$message)
        invokeRestart("muffleWarning")
    }
    
    errorWrapper <- function (error)
    {
        output(OL$Error, error$message, toReport=TRUE)
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

describeExperiment <- function (scriptFile)
{
    scriptFile <- expandFileName(scriptFile)
    inputLines <- readLines(scriptFile)
    outputLines <- paste("OPTIONS for script", scriptFile, "(* required)", sep=" ")
    
    getWithDefault <- function (name, defaultValue, mode = NULL, errorIfMissing = FALSE, errorIfInvalid = FALSE, validValues = NULL)
    {
        leadString <- ifelse(errorIfMissing, " * ", "   ")
        defaultValueString <- ifelse(is.null(defaultValue), "NULL", as.character(defaultValue))
        if (!is.null(validValues))
        {
            otherValues <- (if (is.null(defaultValue)) validValues else validValues[-match(defaultValue,validValues)])
            defaultValueString <- paste(defaultValueString, " [", implode(otherValues,","), "]", sep="")
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
        argsString <- implode(sub("^\\s*\\#\\@args\\s*", "", relevantInputLines, perl=TRUE), ", ")
        outputLines <- c(outputLines, paste("ARGUMENTS:", argsString, sep=" "))
    }
    
    relevantInputLines <- grep("#@desc", inputLines, value=TRUE, fixed=TRUE)
    if (length(relevantInputLines) != 0)
    {
        descriptionString <- implode(sub("^\\s*\\#\\@desc\\s*", "", relevantInputLines, perl=TRUE), " ")
        outputLines <- c(outputLines, descriptionString)
    }
    
    cat(implode(c(outputLines,""), "\n"))
}
