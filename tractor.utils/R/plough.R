ploughExperiment <- function (scriptName, configFiles, variables, tractorFlags, tractorOptions, useGridEngine, crossApply, queueName, qsubOptions, parallelisationFactor, debug)
{
    setOutputLevel(ifelse(isTRUE(debug==1), OL$Debug, OL$Info))
    
    config <- readYaml(configFiles)
    variableLengths <- sapply(config, length)
    
    variables <- splitAndConvertString(variables, ",", fixed=TRUE)
    if (length(variables) == 0 || all(variables == ""))
        variables <- names(config)[variableLengths > 1]
    else if (!all(variables %in% names(config)))
        report(OL$Error, "Specified variable(s) #{implode(variables[!(variables %in% names(config))],sep=', ',finalSep=' and ')} are not mentioned in the config files")
    # otherVariables <- names(config)[!(names(config) %in% variables)]
    
    crossApply <- isTRUE(crossApply == 1)
    useGridEngine <- isTRUE(useGridEngine == 1)
    qsubPath <- locateExecutable("qsub", errorIfMissing=useGridEngine)
    
    usingParallel <- FALSE
    if (isValidAs(parallelisationFactor,"integer") && as.integer(parallelisationFactor) > 1)
    {
        if (useGridEngine)
            report(OL$Warning, "Parallelisation factor will be ignored when using the grid engine")
        else if (system.file(package="parallel") != "")
        {
            library(parallel)
            options(mc.cores=as.integer(parallelisationFactor))
            usingParallel <- TRUE
        }
        else
            report(OL$Warning, "The \"parallel\" package is not installed - code will not be parallelised")
    }
    
    if (crossApply)
    {
        n <- prod(variableLengths[variables])
        data <- as.data.frame(expand.grid(config[variables], stringsAsFactors=FALSE))
        # data <- cbind(data, as.data.frame(lapply(config[!(names(config) %in% variables)], rep, length.out=n)))
    }
    else
    {
        n <- max(variableLengths[variables])
        data <- as.data.frame(lapply(config[variables], rep, length.out=n))
    }
    
    report(OL$Info, "Scheduling #{n} jobs")
    
    setUpRun <- function (i, currentFile)
    {
        currentFlags <- ore.subst("(?<!\\\\)\\%(\\w+)", function(names) data[i,names], tractorFlags, all=TRUE)
        currentFlags <- ore.subst("(?<!\\\\)\\%\\%", i, currentFlags, all=TRUE)
        currentOptions <- ore.subst("(?<!\\\\)\\%(\\w+)", function(names) data[i,names], tractorOptions, all=TRUE)
        currentOptions <- ore.subst("(?<!\\\\)\\%\\%", i, currentOptions, all=TRUE)
        writeYaml(as.list(data[i,]), currentFile, capitaliseLabels=FALSE)
    }
    
    tractorPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "tractor")
    if (useGridEngine)
    {
        for (i in seq_len(n))
        {
            tempDir <- expandFileName("sgetmp")
            if (file.exists(tempDir))
                unlink(tempDir, recursive=TRUE)
            dir.create(file.path(tempDir,"log"), recursive=TRUE)
            dir.create(file.path(tempDir,"output"), recursive=TRUE)
            
            qsubScriptFile <- file.path(tempDir, "script")
            qsubScript <- c("#!/bin/sh",
                            "#$ -S /bin/bash",
                            es("#{tractorPath} -c #{currentFile} #{currentFlags} #{scriptName} #{currentOptions}"))
            writeLines(qsubScript, qsubScriptFile)
        }
    }
    else
    {
        parallelApply(seq_len(n), function(i) {
            currentFile <- threadSafeTempFile()
            setUpRun(i, currentFile)
            execute(tractorPath, es("-c #{currentFile} #{currentFlags} #{scriptName} #{currentOptions}"))
            unlink(currentFile)
        })
    }
}
