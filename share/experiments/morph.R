#@args image file, kernel width
#@desc Apply a mathematical morphology operation to an image using a shaped kernel. This is generally only useful for binary images. Note that the opening operation is an erosion followed by a dilation (using the same kernel), and will tend to remove small objects from the image, while the closing operation is a dilation followed by an erosion, and will tend to close small "holes" in the image. If only one width value is specified then the kernel will be isotropic and have dimensionality equal to that of the image (unless the image has more than three dimensions, in which case it will be 3D). Otherwise the dimensionality of the kernel will be given by the number of values specified. Values can be unequal for an anisotropic kernel.

library(mmand)

runExperiment <- function ()
{
    requireArguments("image file", "kernel width")
    
    operation <- getConfigVariable("Operation", NULL, "character", validValues=c("erode","dilate","opening","closing"), errorIfMissing=TRUE)
    kernelShape <- getConfigVariable("KernelShape", "box", validValues=c("box","disc","diamond"))
    widthUnit <- getConfigVariable("WidthUnit", "vox", validValues=c("mm","vox"))
    
    image <- readImageFile(Arguments[1])
    width <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
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
    
    report(OL$Info, "Applying ", operation, " operation with ", kernelShape, " kernel of size ", implode(round(width)," x "), " voxels")
    
    fileName <- paste(basename(image$getSource()), "morphed", sep="_")
    kernel <- shapeKernel(width, type=kernelShape)
    image$map(get(operation), kernel=kernel)
    writeImageFile(image, fileName)
    
    invisible(NULL)
}
