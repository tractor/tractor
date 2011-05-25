parallelApply <- function (x, fun, ..., preschedule = TRUE, setSeed = TRUE, silent = FALSE, cores = getOption("cores"))
{
    if (exists("mclapply"))
    {
        oldOption <- options(outputPid=TRUE)
        returnValue <- mclapply(x, fun, ..., mc.preschedule=preschedule, mc.set.seed=setSeed, mc.silent=silent, mc.cores=cores)
        options(oldOption)
        return (returnValue)
    }
    else
        return (lapply(x, fun, ...))
}
