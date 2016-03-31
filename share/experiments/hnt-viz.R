#@desc Visualise the results of heuristic neighbourhood tractography, creating image
#@desc volumes and/or projection images of the "best" matching candidate tracts.
#@desc The TractName, ResultsName and SessionList options must match those passed to
#@desc the "hnt-eval" experiment.
#@args [session directory]

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", "results", errorIfMissing=TRUE)
    nStreamlines <- getConfigVariable("Streamlines", 1000L)
    
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
        report(OL$Info, "Generating tract for session ", sessionList[i])
        
        currentSession <- attachMriSession(sessionList[i])
        currentSeed <- results$getResult(attr(sessionList,"indices")[i])$bestSeed
        
        trackerPath <- currentSession$getTracker()$run(currentSeed, nStreamlines, requireMap=TRUE)
        
        currentTractName <- paste(tractName, i, sep=".")
        copyImageFiles(trackerPath, currentTractName)
    })
}
