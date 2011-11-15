#@args image file
#@desc List the key metadata parameters of the specified Analyze, NIfTI or MGH image volume.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    fileName <- implode(Arguments, sep=" ")
    print(newMriImageMetadataFromFile(fileName))
}
