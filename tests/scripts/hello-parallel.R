runExperiment <- function ()
{
    parallelApply(1:2, function(i) {
        cat("Hello, world!\n")
    })
}
