#@args transformation file

library(tractor.reg)

runExperiment <- function ()
{
    requireArguments("transformation file")
    
    transform <- attachTransformation(Arguments[1])
    print(transform$getTransformObjects(1, preferAffine=TRUE))
}
