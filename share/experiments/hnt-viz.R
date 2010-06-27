#@desc Visualise the results of heuristic neighbourhood tractography, creating image
#@desc volumes and/or projection images of the "best" matching candidate tracts.
#@desc The TractName, ResultsName and SessionList options must match those passed to
#@desc the "hnt-eval" experiment.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", "results", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    createVolumes <- getWithDefault("CreateVolumes", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        output(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "hnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        output(OL$Error, "Length of the session list specified does not match the results file")

    parallelApply(seq_len(nSessions), function (i) {
        output(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- results$getResult(i)$bestSeed
        
        ptResult <- runProbtrackWithSession(currentSession, currentSeed, mode="simple", requireImage=TRUE)
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeMriImageToFile(ptResult$image, currentTractName)
        if (createImages)
            writePngsForResult(ptResult, prefix=currentTractName, threshold=vizThreshold, showSeed=showSeed)
    })
}
