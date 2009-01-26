#@desc Extract parameters of interest from HNT results. The specified ResultsName must
#@desc match that given to the "hnt-eval" script. Mode can be "location", for the
#@desc voxel location of the best seed point (using the R convention); or "similarity"
#@desc for the value of the similarity score at that point. In either case, one value
#@desc is given per session.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    mode <- getWithDefault("Mode", NULL, "character", errorIfMissing=TRUE)
    mode <- match.arg(tolower(mode), c("location","similarity"))
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    reference <- deserialiseReferenceTract(refFileName)
    if (!isFieldTract(reference))
        output(OL$Error, "The specified reference tract is not in the correct form")
    
    nSessions <- length(sessionList)
    
    results <- deserialiseHeuristicNTResults(file=ensureFileSuffix(resultsName,"Rdata"))
    if (results$nSessions() != nSessions)
        output(OL$Error, "Length of the session list specified does not match the results file")

    for (i in 1:nSessions)
    {
        output(OL$Info, "Current session is ", sessionList[i])
        result <- results$getResult(i)
        output(OL$Info, "Neighbourhood centre point is ", implode(result$naiveSeed,sep=","))
        
        value <- ifelse(mode=="location", implode(result$bestSeed,sep=","), result$bestSimilarity)
        cat(paste(value, "\n", sep=""))
    }
}
