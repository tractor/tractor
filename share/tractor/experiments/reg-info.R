#@args transform directory
#@desc Report information about the transformations encapsulated within the specified directory (which should have an ".xfmb" extension).
#@group Registration
#@nohistory TRUE

library(tractor.reg)

runExperiment <- function ()
{
    transform <- attachTransformation(Arguments[1])
    setOutputLevel(OL$Info)
    print(transform)
}
