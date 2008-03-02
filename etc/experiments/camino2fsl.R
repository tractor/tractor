#@args source file, reference image
#@desc Convert a Camino data stream to Analyze or NIfTI image format. Since Camino
#@desc files store no image metadata, a separate reference image (in Analyze/NIfTI
#@desc format) must be specified, from which voxel and image dimensions will be
#@desc extracted.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    requireArguments("source file", "reference image")
    metadata <- newMriImageMetadataFromFile(Arguments[2])
    image <- newMriImageFromCamino(Arguments[1], metadata)
    writeMriImageToFile(image)
}
