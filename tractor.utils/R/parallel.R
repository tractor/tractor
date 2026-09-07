#' Apply a function over a vector or list, in parallel if possible
#'
#' This function applies `fun` to each element of `x` in turn, using
#' `parallel::mclapply()` if the `parallel` package is attached, or falling
#' back to a plain [lapply()] call otherwise. When running in parallel, the
#' `reportr` message prefix is temporarily adjusted to include the process
#' ID, so that messages from concurrent workers can be told apart.
#'
#' @param x A vector or list to iterate over.
#' @param fun A function to apply to each element of `x`.
#' @param ... Additional arguments to `fun`.
#' @param preschedule Boolean value passed as `mc.preschedule` to
#'   `parallel::mclapply()`.
#' @param setSeed Boolean value passed as `mc.set.seed` to
#'   `parallel::mclapply()`.
#' @param silent Boolean value passed as `mc.silent` to
#'   `parallel::mclapply()`.
#' @param cores An integer giving the number of cores to use. If `NULL`,
#'   the default, the `mc.cores` or `cores` option is used, in that order
#'   of preference, falling back to two if neither is set.
#' @return A list of results, one per element of `x`, as for [lapply()].
#' @author Jon Clayden
#' @export
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
