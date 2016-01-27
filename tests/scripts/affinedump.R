#@args transformation file

library(tractor.reg)

runExperiment <- function ()
{
    requireArguments("transformation file")
    
    transform <- deserialiseReferenceObject(Arguments[1])
    
    strings <- format(transform$getAffineMatrices(1), digits=5)
    storage.mode(strings) <- "double"
    print(strings)
    
    invisible(NULL)
}
