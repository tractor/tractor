#@args source file, reference image
#@desc Convert a Camino data stream to Analyze, NIfTI or MGH image format. Since Camino files store no image metadata, a separate reference image (in Analyze/NIfTI/MGH format) must be specified, from which voxel and image dimensions will be extracted.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("source file", "reference image")
    metadata <- newMriImageMetadataFromFile(Arguments[2])
    image <- newMriImageFromCamino(Arguments[1], metadata)
    writeMriImageToFile(image)
}
