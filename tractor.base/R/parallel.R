parallelApply <- function (x, fun, ..., preschedule = TRUE, setSeed = TRUE, silent = FALSE, cores = getOption("cores"))
{
    if (exists("mclapply"))
        mclapply(x, fun, ..., mc.preschedule=preschedule, mc.set.seed=setSeed, mc.silent=silent, mc.cores=cores)
    else
        lapply(x, fun, ...)
}
