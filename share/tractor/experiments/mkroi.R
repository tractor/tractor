#@args image file, centre point
#@desc Create an Analyze/NIfTI/MGH volume containing a region of interest with a box, disc or diamond shape. The specified Width may include multiple values and does not need to be the same for each dimension. The output file name is set with the ROIName option.
#@group General analysis

library(tractor.reg)
library(mmand)

runExperiment <- function ()
{
    requireArguments("image file", "centre point")
    
    shape <- getConfigVariable("Shape", "box", validValues=c("box","disc","diamond"))
    width <- getConfigVariable("Width", 7)
    widthUnit <- getConfigVariable("WidthUnit", "vox", validValues=c("mm","vox"))
    roiName <- getConfigVariable("ROIName", "roi")
    
    image <- readImageFile(Arguments[1])
    centre <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    if (length(width) == 1)
        width <- rep(width, min(3,image$getDimensionality()))
    if (widthUnit == "mm")
    {
        if (is.na(image$getVoxelUnits()["spatial"]))
            multiplier <- 1
        else
            multiplier <- switch(image$getVoxelUnits()["spatial"], m=1000, mm=1, um=0.001)
        width <- width / abs(image$getVoxelDimensions()[1:length(width)] * multiplier)
    }
    
    image$fill(0L)
    image[promote(centre,byrow=TRUE)] <- 1L
    kernel <- shapeKernel(width, type=shape)
    image$map(dilate, kernel=kernel)
    writeImageFile(image, roiName)
}
