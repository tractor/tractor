#@args image file, [session directory]
#@desc Calculate the mean or weighted mean value of a metric within the nonzero region of a brain volume (usually tractography output). The specified image can be used as a binary mask (the default) or as a set of weights (with AveragingMode:weighted). In the latter case any weight threshold given is ignored.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("image file")
    image <- readImageFile(Arguments[1])
    
    if (nArguments() > 1)
        session <- newSessionFromDirectory(Arguments[2])
    else
        session <- NULL
    
    metric <- getConfigVariable("Metric", NULL, "character", validValues=c("weight","FA","MD","axialdiff","radialdiff"))
    mode <- getConfigVariable("AveragingMode", "binary", validValues=c("binary","weighted"))
    threshold <- getConfigVariable("WeightThreshold", 0.01)
    thresholdMode <- getConfigVariable("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    
    if (thresholdMode == "maximum")
        threshold <- threshold * max(image, na.rm=TRUE)
    else if (thresholdMode == "minimum")
        threshold <- threshold * min(image, na.rm=TRUE)
    
    images <- createWeightingAndMetricImages(image, session, type=tolower(metric), mode=mode, threshold=threshold)
    finalImage <- images$metric * images$weight
    metric <- sum(finalImage$getData(),na.rm=TRUE) / sum(images$weight$getData(),na.rm=TRUE)
    
    cat(paste(metric, "\n", sep=""))
}
