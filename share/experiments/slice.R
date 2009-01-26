#@args image file, [output file name]
#@desc Create a 2D slice image from the specified Analyze/NIfTI volume. Exactly one
#@desc of the X, Y and Z options must be specified, giving the location on the
#@desc appropriate axis where the slice should be taken.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("image file")
    image <- newMriImageFromFile(Arguments[1])
    
    if (nArguments() > 1)
        outputFile <- Arguments[2]
    else
        outputFile <- image$getSource()
    
    pointType <- getWithDefault("PointType", NULL, "character", errorIfMissing=TRUE)
    x <- getWithDefault("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getWithDefault("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getWithDefault("Z", NA, "numeric", errorIfInvalid=TRUE)
    
    point <- round(c(x,y,z))
    nas <- is.na(point)
    point[nas] <- 1
    
    pointType <- match.arg(tolower(pointType), c("fsl","r","mm"))
    if (pointType == "fsl")
        point <- transformFslVoxelToRVoxel(point)
    else if (pointType == "mm")
        point <- transformWorldToRVoxel(point, image$getMetadata(), useOrigin=TRUE)
    
    point[nas] <- NA
    
    createSliceGraphic(image, point[1], point[2], point[3], device="png", file=outputFile)
}
