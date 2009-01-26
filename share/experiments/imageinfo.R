#@args image file
#@desc List the key metadata parameters of the specified Analyze or NIfTI image volume.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    setOutputLevel(OL$Info, FALSE)
    newMriImageMetadataFromFile(Arguments[1])$summarise()
}
