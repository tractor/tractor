parallelApply <- function (x, fun, ..., preschedule = TRUE, setSeed = TRUE, silent = FALSE, cores = getOption("cores"))
{
    if (exists("mclapply"))
    {
        oldOption <- getOption("reportrPrefixFormat")
        if (is.null(oldOption))
            options(reportrPrefixFormat="[%p] %d%L: ")
        else
            options(reportrPrefixFormat=paste("[%p]",oldOption))
        
        returnValue <- mclapply(x, fun, ..., mc.preschedule=preschedule, mc.set.seed=setSeed, mc.silent=silent, mc.cores=cores)
        
        options(reportrPrefixFormat=oldOption)
        
        return (returnValue)
    }
    else
        return (lapply(x, fun, ...))
}
