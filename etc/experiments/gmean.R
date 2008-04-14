#@args volume prefix
#@desc Calculate the mean or weighted mean value of a metric within the nonzero region
#@desc of a group of brain volumes (usually tractography output) whose file names are
#@desc assumed to have the form of the specified volume prefix followed by a number
#@desc corresponding to the location in the SessionList. The metric can be FA, MD or
#@desc AVF, and the specified image can be used as a binary mask (the default) or as
#@desc a set of weights (with AveragingMode:weighted). In the latter case any weight
#@desc threshold given is ignored.

suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("volume prefix")
    fileStem <- Arguments[1]
    
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    metricName <- getWithDefault("Metric", NULL, "character")
    mode <- getWithDefault("AveragingMode", "binary")
    baseThreshold <- getWithDefault("WeightThreshold", 0.01)
    thresholdMode <- getWithDefault("ThresholdRelativeTo", "nothing")
    
    thresholdMode <- match.arg(tolower(thresholdMode), c("nothing","maximum","minimum"))
    
    for (i in seq_along(sessionList))
    {
        output(OL$Info, "Current session is ", sessionList[i])
        
        session <- newSessionFromDirectory(sessionList[i])
        image <- newMriImageFromFile(paste(fileStem,i,sep=""))
        
        if (thresholdMode == "maximum")
            threshold <- baseThreshold * max(image, na.rm=TRUE)
        else if (thresholdMode == "minimum")
            threshold <- baseThreshold * min(image, na.rm=TRUE)
        else
            threshold <- baseThreshold

        images <- createWeightingAndMetricImages(image, session, type=tolower(metricName), mode=mode, threshold=threshold)
        finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
        metric <- sum(finalImage$getData()) / sum(images$weight$getData())

        cat(paste(metric, "\n", sep=""))
    }
}
