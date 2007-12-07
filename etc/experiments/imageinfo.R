library(tractor.base)

runExperiment <- function ()
{
    requireArguments(1)
    setOutputLevel(OL$Info, FALSE)
    newMriImageMetadataFromFile(Arguments[1])$summarise()
}
