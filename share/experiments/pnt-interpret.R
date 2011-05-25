#@desc Extract parameters of interest from PNT results. Mode can be "location", for the
#@desc voxel location of the best seed point (using the R convention); "posterior", for
#@desc posterior matching probability at that point; "ratio", for the log-ratio of
#@desc likelihoods relative to the reference tract (an indicator of goodness-of-fit);
#@desc or "null-posterior", for the posterior probability of no match. In each case one
#@desc value is given per session.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    originalMaxSeeds <- getConfigVariable("MaximumSeedPoints", 1, "integer")
    minPosterior <- getConfigVariable("MinimumPosterior", 0, "numeric")
    
    mode <- getConfigVariable("Mode", NULL, "character", errorIfMissing=TRUE, c("location","posterior","ratio","null-posterior"), errorIfInvalid=TRUE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    model <- getNTResource("model", "pnt", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        report(OL$Error, "Length of the session list specified does not match the results file")
    nPoints <- results$nPoints()

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        report(OL$Error, "Results file does not describe a cubic search space")
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    seedsInData <- all(c("x","y","z") %in% colnames(data))
    subjectsInData <- ("subject" %in% colnames(data)) && (is.integer(data$subject))
    logLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, model)
    
    refData <- createDataTableForSplines(list(reference$getTract()), reference$getTract(), reference$getTractOptions()$pointType)
    refLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(refData, model)
    
    for (i in 1:nSessions)
    {
        report(OL$Info, "Current session is ", sessionList[i])
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
            currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType=reference$getSeedUnit(), isStandard=TRUE)
        currentPosteriors <- results$getTractPosteriors(i)
        report(OL$Info, "Neighbourhood centre point is ", implode(currentSeed,sep=","))
        
        if (mode == "null-posterior")
        {
            cat(paste(results$getNullPosterior(i), "\n", sep=""))
            next
        }
        
        ranks <- rank(currentPosteriors, na.last="keep")
        maxRank <- max(ranks, na.rm=TRUE)
        ranks <- maxRank + 1 - ranks
        
        if (all(is.na(ranks)))
        {
            report(OL$Warning, "No match data available for session number ", i)
            next
        }
        
        if (maxRank < originalMaxSeeds)
            maxSeeds <- maxRank
        else
            maxSeeds <- originalMaxSeeds
        indices <- match(1:maxSeeds, ranks)
        currentPosteriors[setdiff(1:nPoints,indices)] <- NA
        
        indices <- intersect(indices, which(currentPosteriors>=minPosterior))
        
        if (seedsInData)
            seeds <- as.matrix(currentData[indices,c("x","y","z")])
        else
        {
            neighbourhoodInfo <- createNeighbourhoodInfo(searchWidth, centre=currentSeed)
            seeds <- t(neighbourhoodInfo$vectors[,indices])
        }
        
        for (j in 1:originalMaxSeeds)
        {
            if (j > nrow(seeds))
                cat("NA\n")
            else
            {
                likelihoodIndex <- ((i-1)*nPoints) + indices[j]
                value <- switch(mode, location=implode(seeds[j,],sep=","),
                                      posterior=currentPosteriors[indices[j]],
                                      ratio=(logLikelihoods[likelihoodIndex]-refLogLikelihood))
                cat(paste(value, "\n", sep=""))
            }
        }
    }
}
