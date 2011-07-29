#@desc Run "pnt-data" on a multicore computer or grid using the Sun Grid Engine (SGE). Options are generally the same as "pnt-data", and are passed through to it. Temporary files created during the process (in a directory called "sgetmp") will be removed at the end unless KeepAllOutput:true is given. The options QueueName, MainJobOptions and CollateJobOptions can be used to pass extra parameters to SGE.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    seedList <- getConfigVariable("SeedPointList", NULL, "integer")
    pointType <- getConfigVariable("PointType", NULL, "character")
    searchWidth <- getConfigVariable("SearchWidth", 1)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.2)
    nSamples <- getConfigVariable("NumberOfSamples", 1000)
    datasetName <- getConfigVariable("DatasetName", "data")
    sessionNumbers <- getConfigVariable("SessionNumbers", NULL, "character")
    
    queueName <- getConfigVariable("QueueName", NULL, "character")
    keepOutput <- getConfigVariable("KeepAllOutput", FALSE)
    mainJobOptions <- getConfigVariable("MainJobOptions", NULL, "character")
    collateJobOptions <- getConfigVariable("CollateJobOptions", NULL, "character")
    
    tractorLoc <- locateExecutable("tractor", errorIfMissing=TRUE)
    qsubLoc <- locateExecutable("qsub", errorIfMissing=TRUE)
    
    if (!keepOutput)
        rmLoc <- locateExecutable("rm", errorIfMissing=TRUE)
    
    if (is.null(sessionNumbers))
        sessionNumbers <- seq_along(sessionList)
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)

    tempDir <- expandFileName("sgetmp")
    if (file.exists(tempDir))
        unlink(tempDir, recursive=TRUE)
    dir.create(file.path(tempDir,"log"), recursive=TRUE)
    dir.create(file.path(tempDir,"output"), recursive=TRUE)
    
    sessionFile <- file.path(tempDir, "sessions")
    idFile <- file.path(tempDir, "ids")
    writeLines(sessionList, sessionFile)
    writeLines(as.character(sessionNumbers), idFile)
    
    if (!is.null(seedList))
    {
        seedMatrix <- matrix(seedList, ncol=3, byrow=TRUE)
        seedLocs <- apply(seedMatrix, 1, implode, sep=",")
        seedFile <- file.path(tempDir, "seeds")
        writeLines(seedLocs, seedFile)
    }
    
    verbosityFlag <- ifelse(isValidAs(Sys.getenv("verbose_level"),"integer"), paste("-v",Sys.getenv("verbose_level")), "")
    
    carryOverOptions <- paste("TractName:", tractName, " Tracker:", tracker, " AnisotropyThreshold:", faThreshold, " NumberOfSamples:", nSamples, " SearchWidth:", searchWidth, " DatasetName:", file.path(tempDir,"output",datasetName), sep="")
    script <- c("#!/bin/sh",
                "#$ -S /bin/bash",
                paste("SESSION=`sed \"${SGE_TASK_ID}q;d\" ",sessionFile,"`",sep=""),
                paste("NUMBER=`sed \"${SGE_TASK_ID}q;d\" ",idFile,"`",sep=""))
    if (is.null(seedList))
    {
        script <- c(script,
                    paste(tractorLoc, " -q -z ", verbosityFlag, " pnt-data SessionList:${SESSION} SessionNumbers:${NUMBER} ", carryOverOptions, sep=""))
    }
    else
    {
        carryOverOptions <- paste(carryOverOptions, " PointType:", pointType, sep="")
        script <- c(script,
                    paste("SEED=`sed \"${SGE_TASK_ID}q;d\" ",seedFile,"`",sep=""),
                    paste(tractorLoc, " -q -z ", verbosityFlag, " pnt-data SessionList:${SESSION} SessionNumbers:${NUMBER} SeedPoint:${SEED} ", carryOverOptions, sep=""))
    }
    scriptFile <- file.path(tempDir, "script")
    writeLines(script, scriptFile)
    execute("chmod", paste("+x",scriptFile))
    
    report(OL$Info, "Submitting jobs to SGE")
    queueOption <- ifelse(is.null(queueName), "", paste("-q",queueName))
    qsubArgs <- paste("-terse -V -wd", path.expand(getwd()), queueOption, "-N pnt-data -o", file.path(tempDir,"log"), "-e /dev/null", paste("-t 1-",length(sessionList),sep=""), mainJobOptions, scriptFile)
    result <- execute("qsub", qsubArgs, intern=TRUE)
    jobNumber <- as.numeric(sub("^(\\d+)\\..+$", "\\1", result, perl=TRUE))
    report(OL$Info, "Main job number is ", jobNumber)
    
    carryOverOptions <- paste("TractName:", tractName, " DatasetName:", datasetName, " SearchWidth:", searchWidth, sep="")
    fileStem <- file.path(tempDir, "output", paste(datasetName,"session",sep="_"))
    qsubArgs <- paste("-terse -V -wd", path.expand(getwd()), queueOption, "-N pnt-collate -o", file.path(tempDir,"log"), "-e /dev/null -hold_jid", jobNumber, collateJobOptions, tractorLoc, "-q -z", verbosityFlag, "pnt-collate", fileStem, carryOverOptions)
    result <- execute("qsub", qsubArgs, intern=TRUE)
    jobNumber <- as.numeric(sub("^(\\d+)\\..+$", "\\1", result, perl=TRUE))
    report(OL$Info, "Collation job number is ", jobNumber)
    
    if (!keepOutput)
    {
        qsubArgs <- paste("-terse -V -N cleanup -o /dev/null -e /dev/null -b yes -hold_jid", jobNumber, rmLoc, "-r", tempDir)
        execute("qsub", qsubArgs, silent=TRUE)
    }
    
    return (invisible(NULL))
}
