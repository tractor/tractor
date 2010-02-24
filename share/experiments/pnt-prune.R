#@desc Visualise the results of evaluating a data set against a PNT model, creating
#@desc Analyze/NIfTI volumes (with CreateVolumes:true) and/or projection images
#@desc (CreateImages:true) of the best matching tracts for each session. A single
#@desc seed point is used in each case, but individual streamlines generated from
#@desc this seed are retained or rejected depending on their likelihood under the
#@desc model.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    modelName <- getWithDefault("ModelName", NULL, "character")
    sessionNumbers <- getWithDefault("SessionNumbers", NULL, "character")
    
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    subgroupSize <- getWithDefault("SubgroupSize", 500)
    truncate <- getWithDefault("TruncateToReference", TRUE)
    randomSeed <- getWithDefault("RandomSeed", NULL, "integer")
    
    createVolumes <- getWithDefault("CreateVolumes", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    refSession <- reference$getSourceSession()
    options <- reference$getTractOptions()

    if (!is.null(randomSeed))
        set.seed(randomSeed)

    model <- getNTResource("model", "pnt", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    
    if (!createVolumes && !createImages)
        output(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    options(bitmapType="Xlib")
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        output(OL$Error, "Length of the session list specified does not match the results file")
    nPoints <- results$nPoints()

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        output(OL$Error, "Results file does not describe a cubic search space")
    
    if (is.null(sessionNumbers))
        sessionNumbers <- 1:nSessions
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    for (i in sessionNumbers)
    {
        output(OL$Info, "Generating tract for session ", i)
        
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType=reference$getSeedUnit(), isStandard=TRUE)
        currentPosteriors <- results$getTractPosteriors(i)
        
        bestSeedIndex <- which.max(currentPosteriors)
        if (length(bestSeedIndex) == 0)
        {
            output(OL$Warning, "No match data available for session number ", i)
            next
        }
        
        neighbourhood <- createNeighbourhoodInfo(searchWidth, centre=currentSeed)
        bestSeed <- neighbourhood$vectors[,bestSeedIndex]
        
        streamSet <- newStreamlineSetTractFromProbtrack(currentSession, bestSeed, nSamples=nSamples)
        
        medianLine <- newStreamlineTractFromSet(streamSet, method="median", lengthQuantile=options$lengthQuantile, originAtSeed=TRUE)
        if (options$registerToReference)
        {
            if (is.null(refSession))
                transform <- newAffineTransform3DByInversion(getMniTransformForSession(currentSession))
            else
                transform <- newAffineTransform3DFromFlirt(currentSession$getImageFileNameByType("t2"), refSession$getImageFileNameByType("t2"))

            transformedMedianLine <- newStreamlineTractByTransformation(medianLine, transform)
        }
        medianSpline <- newBSplineTractFromStreamline(transformedMedianLine, knotSpacing=options$knotSpacing)
        medianData <- createDataTableForSplines(list(medianSpline), reference$getTract(), "knot")
        medianLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(medianData, model)
        output(OL$Info, "Median line log-likelihood is ", round(medianLogLikelihood,3))

        data <- NULL
        nGroups <- (nSamples - 1) %/% subgroupSize + 1

        output(OL$Info, "Creating complete data table...")
        for (j in 1:nGroups)
        {
            firstStreamline <- subgroupSize * (j-1) + 1
            lastStreamline <- min(j*subgroupSize, nSamples)
            streamSubset <- newStreamlineSetTractBySubsetting(streamSet, firstStreamline:lastStreamline)
            splines <- calculateSplinesForStreamlineSetTract(streamSubset, currentSession, refSession, options)
            data <- rbind(data, createDataTableForSplines(splines,reference$getTract(),"knot"))
            rm(splines)
        }

        logLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, model)
        logLikelihoodRatios <- logLikelihoods - medianLogLikelihood
        keepProbabilities <- exp(pmin(logLikelihoodRatios,0))
        toAccept <- which(!is.na(keepProbabilities) & runif(length(keepProbabilities)) <= keepProbabilities)
        toReject <- setdiff(1:nSamples, toAccept)
        
        if (length(toAccept) == 0)
        {
            output(OL$Warning, "All streamlines rejected for session ", i, " - using median line")
            lineMetadata <- newStreamlineTractMetadataFromImageMetadata(medianLine$getImageMetadata(), FALSE, "vox")
            medianLine <- newStreamlineTractWithMetadata(medianLine, lineMetadata)
            streamSet <- newStreamlineSetTractFromStreamline(medianLine)
        }
        else
        {
            output(OL$Info, "Rejecting ", length(toReject), " streamlines, of which ", sum(is.na(keepProbabilities)), " are missing")
            streamSet <- newStreamlineSetTractBySubsetting(streamSet, toAccept)
        }
        
        if (truncate)
        {
            output(OL$Info, "Truncating streamlines to the length of the reference tract")
            streamSet <- newStreamlineSetTractByTruncationToReference(streamSet, reference, currentSession)
        }
        
        output(OL$Info, "Creating visitation map")
        metadata <- newMriImageMetadataFromFile(currentSession$getImageFileNameByType("fa"))
        visitationMap <- newMriImageAsVisitationMap(streamSet, metadata)
        fakeResult <- list(image=visitationMap, nSamples=streamSet$nStreamlines(), session=currentSession, seed=bestSeed)
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeMriImageToFile(visitationMap, currentTractName)
        if (createImages)
            writePngsForResult(fakeResult, prefix=currentTractName, threshold=1/(streamSet$nStreamlines()+1), showSeed=showSeed)
    }
}
