#@args image file
#@desc Print various statistics of the specified image, optionally limiting the calculations by voxel value or by mask. The 5% trimmed range is obtained by discarding the 5% largest and smallest values and taking the range of the remainder, thereby giving an indication of the influence of extreme values. This script may currently only be applied to 3D images.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    
    fileName <- implode(Arguments, sep=" ")
    image <- newMriImageFromFile(fileName)
    
    scope <- getConfigVariable("Scope", "all", validValues=c("nonzero","zero","positive","negative","all"))
    maskFile <- getConfigVariable("MaskFile", NULL, "character")
    threshold <- getConfigVariable("IntensityThreshold", 0)
    
    if (!is.null(maskFile))
    {
        mask <- newMriImageFromFile(maskFile)
        if (!equivalent(image$getDimensions(), mask$getDimensions()))
            report(OL$Error, "Mask dimensions do not match those of the main image")
        if (!equivalent(image$getVoxelDimensions(), mask$getVoxelDimensions()))
            report(OL$Error, "Mask voxel dimensions do not match those of the main image")
    }
    else
        mask <- NULL
    
    if (image$getDimensionality() != 3)
        report(OL$Error, "Only 3D images can be used at present")
    
    spatialUnit <- image$getVoxelUnit()[image$getVoxelUnit() %~% "m$"]
    if (length(spatialUnit) == 0)
        volumeUnit <- ""
    else
        volumeUnit <- paste(spatialUnit, "^3", sep="")
    
    data <- as.vector(image$getData())
    if (!is.null(mask))
        data <- data[mask$getData() != 0]
    if (scope != "all")
    {
        operator <- switch(scope, nonzero="!=", zero="==", positive=">", negative="<")
        data <- data[get(operator)(data, 0)]
    }
    if (threshold > 0)
        data <- data[data >= threshold]
    
    if (length(data) == 0)
    {
        labels <- "Number of voxels"
        values <- as.character(0)
    }
    else
    {
        labels <- c("Number of voxels", "Volume", "Intensity range", "5% trimmed range", "Mean", "5% trimmed mean", "Median", "Standard deviation")
        values <- c(format(length(data),big.mark=","), paste(format(round(abs(length(data)*prod(image$getVoxelDimensions())),2),big.mark=","),volumeUnit), implode(signif(range(data),4),sep=" to "), implode(signif(quantile(data,c(0.05,0.95)),4),sep=" to "), signif(mean(data),4), signif(mean(data,trim=0.05),4), signif(median(data),4), signif(sd(data),4))
    }
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    printLabelledValues(labels, values)
}
