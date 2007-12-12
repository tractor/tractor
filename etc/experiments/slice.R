suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments(1)
    image <- newMriImageFromFile(Arguments[1])
    
    if (nArguments() > 1)
        outputFile <- Arguments[2]
    else
        outputFile <- image$getSource()
    
    x <- getWithDefault("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getWithDefault("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getWithDefault("Z", NA, "numeric", errorIfInvalid=TRUE)
    
    createSliceGraphic(image, x, y, z, device="png", file=outputFile)
}
