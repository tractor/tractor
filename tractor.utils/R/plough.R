ploughExperiment <- function (scriptName, configFiles, variables, tractorFlags, tractorOptions, useGridEngine, crossApply, queueName, qsubOptions, parallelisationFactor)
{
    setOutputLevel(Info)
    
    config <- readYaml(configFiles)
    variableLengths <- sapply(config, length)
    
    crossApply <- all(crossApply == 1)
    useGridEngine <- all(useGridEngine == 1)
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
}
