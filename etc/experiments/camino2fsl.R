#@args source file, reference image

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    requireArguments("source file", "reference image")
    metadata <- newMriImageMetadataFromFile(Arguments[2])
    image <- newMriImageFromCamino(Arguments[1], metadata)
    writeMriImageToFile(image)
}
