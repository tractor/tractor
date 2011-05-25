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
    
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    x <- getConfigVariable("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getConfigVariable("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getConfigVariable("Z", NA, "numeric", errorIfInvalid=TRUE)
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            report(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    point <- round(c(x,y,z))
    nas <- is.na(point)
    point[nas] <- 1
    
    if (pointType == "fsl")
        point <- transformFslVoxelToRVoxel(point)
    else if (pointType == "mm")
        point <- transformWorldToRVoxel(point, image$getMetadata(), useOrigin=TRUE)
    
    point[nas] <- NA
    
    createSliceGraphic(image, point[1], point[2], point[3], device="png", file=outputFile, windowLimits=windowLimits)
}
