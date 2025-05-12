#@args registration file

library(tractor.reg)

runExperiment <- function ()
{
    requireArguments("registration file")
    
    transform <- readRegistration(Arguments[1])
    print(transform$getTransforms(1, preferAffine=TRUE))
}
