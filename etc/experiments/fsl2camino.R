#@args source file
#@desc Convert an Analyze or NIfTI image file to a data stream for use with Camino.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    requireArguments("source file")
    image <- newMriImageFromFile(Arguments[1])
    fileName <- writeMriImageToCamino(image, image$getSource())
    return (list(fileName=fileName))
}
