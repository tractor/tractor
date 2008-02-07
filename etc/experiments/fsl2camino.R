suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    requireArguments("source file")
    image <- newMriImageFromFile(Arguments[1])
    writeMriImageToCamino(image, image$getSource())
}
