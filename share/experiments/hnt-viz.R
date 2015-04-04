#@desc Visualise the results of heuristic neighbourhood tractography, creating image
#@desc volumes and/or projection images of the "best" matching candidate tracts.
#@desc The TractName, ResultsName and SessionList options must match those passed to
#@desc the "hnt-eval" experiment.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", "results", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "hnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        report(OL$Error, "Length of the session list specified does not match the results file")

    parallelApply(seq_len(nSessions), function (i) {
        report(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- results$getResult(i)$bestSeed
        
        ptResult <- trackWithSession(currentSession, currentSeed, requireImage=TRUE)
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeImageFile(ptResult$image, currentTractName)
        if (createImages)
            writePngsForResult(ptResult, prefix=currentTractName, threshold=vizThreshold, showSeed=showSeed)
    })
}
