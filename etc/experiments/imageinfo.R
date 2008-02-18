#@args image file

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    setOutputLevel(OL$Info, FALSE)
    newMriImageMetadataFromFile(Arguments[1])$summarise()
}
