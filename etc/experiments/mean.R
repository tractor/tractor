suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("image file")
    image <- newMriImageFromFile(Arguments[1])
    
    if (nArguments() > 1)
        session <- newSessionFromDirectory(Arguments[2])
    else
        session <- NULL
    
    metric <- getWithDefault("Metric", NULL, "character")
    mode <- getWithDefault("AveragingMode", "binary")
    threshold <- getWithDefault("WeightThreshold", 0.01)
    thresholdMode <- getWithDefault("ThresholdRelativeTo", "nothing")
    
    thresholdMode <- match.arg(thresholdMode, c("nothing","maximum","minimum"))
    if (thresholdMode == "maximum")
        threshold <- threshold * max(image, na.rm=TRUE)
    else if (thresholdMode == "minimum")
        threshold <- threshold * min(image, na.rm=TRUE)
    
    images <- createWeightingAndMetricImages(image, session, type=tolower(metric), mode=mode, threshold=threshold)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    metric <- sum(finalImage$getData()) / sum(images$weight$getData())
    
    cat(paste(metric, "\n", sep=""))
}
