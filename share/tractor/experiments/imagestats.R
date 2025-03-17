#@args image file
#@desc Print various statistics of the specified image, optionally limiting the calculations by voxel value or by mask. The 5% trimmed range is obtained by discarding the 5% largest and smallest values and taking the range of the remainder, thereby giving an indication of the influence of extreme values. This script may currently only be applied to 3D images.
#@group General analysis
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    
    fileName <- implode(Arguments, sep=" ")
    image <- readImageFile(fileName)
    
    scope <- getConfigVariable("Scope", "nonzero", validValues=c("nonzero","zero","positive","negative","all"))
    maskFile <- getConfigVariable("MaskFile", NULL, "character")
    threshold <- getConfigVariable("ThresholdLevel", 0)
    
    if (!is.null(maskFile))
    {
        mask <- readImageFile(maskFile)
        if (!equivalent(image$getDimensions(), mask$getDimensions()))
            report(OL$Error, "Mask dimensions do not match those of the main image")
        if (!equivalent(image$getVoxelDimensions(), mask$getVoxelDimensions()))
            report(OL$Error, "Mask voxel dimensions do not match those of the main image")
    }
    else
        mask <- NULL
    
    if (image$getDimensionality() != 3)
        report(OL$Error, "Only 3D images can be used at present")
    
    spatialUnit <- image$getVoxelUnits()["spatial"]
    if (is.na(spatialUnit))
    {
        volumeUnit <- ""
        millilitreMultiplier <- NA
    }
    else
    {
        volumeUnit <- paste(spatialUnit, "^3", sep="")
        millilitreMultiplier <- switch(spatialUnit, m=1e6, mm=1e-3, um=1e-12)
    }
    
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
    
    data <- na.omit(data)
    
    if (length(data) == 0)
    {
        labels <- "Number of voxels"
        values <- as.character(0)
    }
    else
    {
        volume <- abs(length(data) * prod(image$getVoxelDimensions()))
        volumeString <- paste(format(round(volume,2), big.mark=","), volumeUnit)
        if (!is.na(millilitreMultiplier))
            volumeString <- paste(volumeString, " (", format(round(volume*millilitreMultiplier,2), big.mark=","), " ml)", sep="")
        
        labels <- c("Scope", "Number of voxels", "Volume", "Intensity range", "5% trimmed range", "Mean", "5% trimmed mean", "Median", "Standard deviation")
        values <- c(scope, format(length(data),big.mark=","), volumeString, implode(signif(range(data),4),sep=" to "), implode(signif(quantile(data,c(0.05,0.95)),4),sep=" to "), signif(mean(data),4), signif(mean(data,trim=0.05),4), signif(median(data),4), signif(sd(data),4))
    }
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    printLabelledValues(labels, values)
}
