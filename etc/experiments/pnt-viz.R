#@desc Visualise the results of evaluating a data set against a PNT model, creating
#@desc Analyze/NIfTI volumes (with CreateVolumes:true) and/or projection images
#@desc (CreateImages:true) of the best matching tracts for each session. The number
#@desc of seed points contributing to the final tract is affected by the options
#@desc MaximumSeedPoints and MinimumPosterior. In the default case a single seed
#@desc point will always be used, but with MaximumSeedPoints:5 MinimumPosterior:0.01,
#@desc for example, then up to 5 seeds will contribute, if that many have matching
#@desc posterior probabilities of at least 0.01.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    maxSeeds <- getWithDefault("MaximumSeedPoints", 1, "integer")
    minPosterior <- getWithDefault("MinimumPosterior", 0, "numeric")
    
    createVolumes <- getWithDefault("CreateVolumes", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    reference <- deserialiseReferenceTract(refFileName)
    if (!isBSplineTract(reference))
        output(OL$Error, "The specified reference tract is not in the correct form")
    
    if (!createVolumes && !createImages)
        output(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    options(bitmapType="Xlib")
    
    nSessions <- length(sessionList)
    
    results <- deserialiseProbabilisticNTResults(file=ensureFileSuffix(resultsName,"Rdata"))
    if (results$nSessions() != nSessions)
    {
        nSessions <- min(nSessions, results$nSessions())
        output(OL$Warning, "Length of the session list does not match the results file - using ", nSessions, " sessions only")
    }
    nPoints <- results$nPoints()

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        output(OL$Error, "Results file does not describe a cubic search space")
    
    if (vizThreshold == 0)
        vizThreshold <- NULL
    
    for (i in 1:nSessions)
    {
        output(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType=reference$getSeedUnit(), isStandard=TRUE)
        currentPosteriors <- results$getTractPosteriors(i)
        
        ranks <- rank(currentPosteriors, na.last="keep")
        maxRank <- max(ranks, na.rm=TRUE)
        ranks <- maxRank + 1 - ranks
        
        if (all(is.na(ranks)))
        {
            output(OL$Warning, "No match data available for session number ", i)
            next
        }
        
        if (maxRank < maxSeeds)
            maxSeeds <- maxRank
        indices <- match(1:maxSeeds, ranks)
        currentPosteriors[setdiff(1:nPoints,indices)] <- NA
        
        ptResult <- runProbtrackForNeighbourhood(currentSession, currentSeed, width=searchWidth, weights=currentPosteriors, weightThreshold=minPosterior, requireImage=TRUE)
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeMriImageToFile(ptResult$image, currentTractName)
        if (createImages)
            writePngsForResult(ptResult, prefix=currentTractName, threshold=vizThreshold, showSeed=showSeed)
    }
}
