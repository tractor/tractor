suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    refTractFile <- getWithDefault("ReferenceTractFile", NULL, "character", errorIfMissing=TRUE)
    resultsFile <- getWithDefault("ResultsFile", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    maxSeeds <- getWithDefault("MaximumSeedPoints", 1, "integer")
    minPosterior <- getWithDefault("MinimumPosterior", 0, "numeric")
    
    createVolumes <- getWithDefault("CreateVolumes", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    tractName <- getWithDefault("TractName", "tract")
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    if (!all(c("seed","spline","options") %in% load(refTractFile)))
        output(OL$Error, "The file specified does not seem to contain reference tract information")
    
    if (!createVolumes && !createImages)
        output(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    nSessions <- length(sessionList)
    
    load(resultsFile)
    if (length(tp) != nSessions)
        output(OL$Error, "Length of the session list specified does not match the results file")
    nPoints <- length(tp[[1]])

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        output(OL$Error, "Results file does not describe a cubic search space")
    
    for (i in 1:nSessions)
    {
        output(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- getNativeSpacePointForSession(currentSession, seed, pointType="r", isStandard=TRUE)
        currentPosteriors <- tp[[i]]
        
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
