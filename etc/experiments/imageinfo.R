library(tractor.base)

runExperiment <- function ()
{
    requireArguments(1)
    setOutputLevel(OL$Info)
    newMriImageFromFile(Arguments[1])$summarise()
}
