#@args source file

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    requireArguments("source file")
    image <- newMriImageFromFile(Arguments[1])
    fileName <- writeMriImageToCamino(image, image$getSource())
    return (list(fileName=fileName))
}
