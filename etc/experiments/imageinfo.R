library(tractor.base)

runExperiment <- function ()
{
    requireArguments(1)
    setOutputLevel(OL$Info)
    newMriImageMetadataFromFile(Arguments[1])$summarise()
}
