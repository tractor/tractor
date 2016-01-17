#@args volume prefix
#@desc Calculate the mean or weighted mean value of a metric within the nonzero region of a group of brain volumes (usually tractography output) whose file names are assumed to have the form of the specified volume prefix followed by a number corresponding to the location in the SessionList. The specified image can be used as a binary mask (the default) or as a set of weights (with AveragingMode:weighted). In the latter case any weight threshold given is ignored.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("volume prefix")
    fileStem <- Arguments[1]
    
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    metricName <- getConfigVariable("Metric", NULL, "character", validValues=c("weight","FA","MD","axialdiff","radialdiff"))
    mode <- getConfigVariable("AveragingMode", "binary", validValues=c("binary","weighted"))
    baseThreshold <- getConfigVariable("WeightThreshold", 0.01)
    thresholdMode <- getConfigVariable("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    
    for (i in seq_along(sessionList))
    {
        report(OL$Info, "Current session is ", sessionList[i])
        
        fileName <- paste(fileStem, i, sep="")
        if (!imageFileExists(fileName))
        {
            cat("NA\n")
            next
        }
        
        session <- newSessionFromDirectory(sessionList[i])
        image <- readImageFile(fileName)
        
        if (thresholdMode == "maximum")
            threshold <- baseThreshold * max(image, na.rm=TRUE)
        else if (thresholdMode == "minimum")
            threshold <- baseThreshold * min(image, na.rm=TRUE)
        else
            threshold <- baseThreshold

        images <- createWeightingAndMetricImages(image, session, type=tolower(metricName), mode=mode, threshold=threshold)
        finalImage <- images$metric * images$weight
        metric <- sum(finalImage$getData(),na.rm=TRUE) / sum(images$weight$getData(),na.rm=TRUE)

        cat(paste(metric, "\n", sep=""))
    }
}
