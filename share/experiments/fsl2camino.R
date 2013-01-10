#@args source file
#@desc Convert an Analyze, NIfTI or MGH image file to a data stream for use with Camino.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("source file")
    image <- readImageFile(Arguments[1])
    fileName <- writeMriImageToCamino(image, image$getSource())
    return (list(fileName=fileName))
}
