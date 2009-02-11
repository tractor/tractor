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
