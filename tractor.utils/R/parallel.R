parallelApply <- function (x, fun, ..., preschedule = TRUE, setSeed = TRUE, silent = FALSE, cores = NULL)
{
    if (exists("mclapply"))
    {
        if (is.null(cores))
            cores <- c(getOption("mc.cores"), getOption("cores"), 2L)[1]
        
        oldOption <- getOption("reportrPrefixFormat")
        if (is.null(oldOption))
            options(reportrPrefixFormat="[%p] %d%L: ")
        else
            options(reportrPrefixFormat=paste("[%p]",oldOption))
        
        returnValue <- mclapply(x, fun, ..., mc.preschedule=preschedule, mc.set.seed=setSeed, mc.silent=silent, mc.cores=cores, mc.cleanup=TRUE)
        
        options(reportrPrefixFormat=oldOption)
        
        return (returnValue)
    }
    else
        return (lapply(x, fun, ...))
}
