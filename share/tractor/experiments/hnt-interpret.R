#@desc Extract parameters of interest from HNT results. The specified ResultsName must match that given to the "hnt-eval" script. Mode can be "location", for the voxel location of the best seed point (using the R convention); or "similarity" for the value of the similarity score at that point. If a session directory is not specified then a value will be given for each session path stored in the results file.
#@args [session directory]

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    mode <- getConfigVariable("Mode", NULL, "character", errorIfMissing=TRUE, validValues=c("location","similarity"), errorIfInvalid=TRUE)
    
    results <- getNTResource("results", "hnt", list(resultsName=resultsName))
    
    if (nArguments() > 0)
        sessionList <- matchPaths(Arguments, results$getSessionPaths())
    else
    {
        sessionList <- results$getSessionPaths()
        attr(sessionList, "indices") <- 1:length(sessionList)
    }

    for (i in seq_along(sessionList))
    {
        report(OL$Info, "Current session is ", sessionList[i])
        result <- results$getResult(attr(sessionList,"indices")[i])
        report(OL$Info, "Neighbourhood centre point is (#{implode(result$naiveSeed,sep=',')})")
        
        value <- ifelse(mode=="location", implode(result$bestSeed,sep=","), result$bestSimilarity)
        cat(paste(value, "\n", sep=""))
    }
}
