#@args metric image, [mask image]
#@desc Calculate the mean or weighted mean value of a metric within the nonzero region of a brain volume. The specified mask image can be used as a binary mask (the default) or as a set of weights (with AveragingMode:weighted). In the latter case any weight threshold given is ignored. If the mask is missing then the metric image is itself the mask.

runExperiment <- function ()
{
    requireArguments("metric image")
    metricImage <- readImageFile(Arguments[1])
    
    if (nArguments() > 1)
        maskImage <- readImageFile(Arguments[2])
    else
        maskImage <- metricImage$copy()
    
    mode <- getConfigVariable("AveragingMode", "binary", validValues=c("binary","weighted"))
    threshold <- getConfigVariable("ThresholdLevel", 0.01)
    thresholdMode <- getConfigVariable("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    
    if (thresholdMode == "maximum")
        threshold <- threshold * max(maskImage, na.rm=TRUE)
    else if (thresholdMode == "minimum")
        threshold <- threshold * min(maskImage, na.rm=TRUE)
    
    if (mode == "binary")
        maskImage$threshold(threshold)$binarise()
    
    metric <- sum(metricImage * maskImage, na.rm=TRUE) / sum(maskImage, na.rm=TRUE)
    cat(paste(metric, "\n", sep=""))
}
