#@desc Extract parameters of interest from HNT results. The specified ResultsName must
#@desc match that given to the "hnt-eval" script. Mode can be "location", for the
#@desc voxel location of the best seed point (using the R convention); or "similarity"
#@desc for the value of the similarity score at that point. In either case, one value
#@desc is given per session.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    mode <- getConfigVariable("Mode", NULL, "character", errorIfMissing=TRUE, validValues=c("location","similarity"), errorIfInvalid=TRUE)
    
    reference <- getNTResource("reference", "hnt", list(tractName=tractName))
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "hnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        report(OL$Error, "Length of the session list specified does not match the results file")

    for (i in 1:nSessions)
    {
        report(OL$Info, "Current session is ", sessionList[i])
        result <- results$getResult(i)
        report(OL$Info, "Neighbourhood centre point is ", implode(result$naiveSeed,sep=","))
        
        value <- ifelse(mode=="location", implode(result$bestSeed,sep=","), result$bestSimilarity)
        cat(paste(value, "\n", sep=""))
    }
}
