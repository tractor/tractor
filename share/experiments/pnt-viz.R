#@desc Visualise the results of evaluating a data set against a PNT model, creating Analyze/NIfTI/MGH volumes (with CreateVolumes:true) and/or projection images (CreateImages:true) of the best matching tracts for each session. The number of seed points contributing to the final tract is affected by the options MaximumSeedPoints and MinimumPosterior. In the default case a single seed point will always be used, but with MaximumSeedPoints:5 MinimumPosterior:0.01, for example, then up to 5 seeds will contribute, if that many have matching posterior probabilities of at least 0.01.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character")
    
    maxSeeds <- getConfigVariable("MaximumSeedPoints", 1, "integer")
    minPosterior <- getConfigVariable("MinimumPosterior", 0, "numeric")
    nSamples <- getConfigVariable("NumberOfSamples", 1000, "integer")
    
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
    {
        nSessions <- min(nSessions, results$nSessions())
        report(OL$Warning, "Length of the session list does not match the results file - using ", nSessions, " sessions only")
    }
    nPoints <- results$nPoints()

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        report(OL$Error, "Results file does not describe a cubic search space")
    
    if (vizThreshold == 0)
        vizThreshold <- NULL
    
    if (is.null(datasetName))
        seedsInData <- FALSE
    else
    {
        data <- read.table(ensureFileSuffix(datasetName,"txt"))
        seedsInData <- all(c("x","y","z") %in% colnames(data))
        subjectsInData <- ("subject" %in% colnames(data)) && (is.integer(data$subject))
    }
    
    parallelApply(seq_len(nSessions), function (i) {
        report(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        if (seedsInData)
        {
            if (subjectsInData)
                currentData <- subset(data, subject==i)
            else
                currentData <- data[(((i-1)*nPoints)+1):(i*nPoints),]
            currentSeed <- round(apply(currentData[,c("x","y","z")], 2, median))
        }
        else
            currentSeed <- transformPointsToSpace(reference$getStandardSpaceSeedPoint(), currentSession, "diffusion", oldSpace="mni", reverseRegister=TRUE, pointType=reference$getSeedUnit(), outputVoxel=TRUE, nearest=TRUE)
        
        currentPosteriors <- results$getTractPosteriors(i)
        
        ranks <- rank(currentPosteriors, na.last="keep")
        maxRank <- max(ranks, na.rm=TRUE)
        ranks <- maxRank + 1 - ranks
        
        if (all(is.na(ranks)))
        {
            report(OL$Warning, "No match data available for session number ", i)
            return (invisible(NULL))
        }
        
        if (maxRank < maxSeeds)
            maxSeeds <- maxRank
        indices <- match(1:maxSeeds, ranks)
        currentPosteriors[setdiff(1:nPoints,indices)] <- NA
        
        ptResult <- runProbtrackForNeighbourhood(currentSession, currentSeed, width=searchWidth, weights=currentPosteriors, weightThreshold=minPosterior, nSamples=nSamples, requireImage=TRUE)
        if (is.null(ptResult))
        {
            report(OL$Warning, "No seed points above threshold for session number ", i)
            return (invisible(NULL))
        }
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeImageFile(ptResult$image, currentTractName)
        if (createImages)
            writePngsForResult(ptResult, prefix=currentTractName, threshold=vizThreshold, showSeed=showSeed)
    })
}
