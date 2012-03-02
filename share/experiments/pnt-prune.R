#@desc Visualise the results of evaluating a data set against a PNT model, creating Analyze/NIfTI/MGH volumes (with CreateVolumes:true) and/or projection images (CreateImages:true) of the best matching tracts for each session. A single seed point is used in each case, but individual streamlines generated from this seed are retained or rejected depending on their likelihood under the model.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    sessionNumbers <- getConfigVariable("SessionNumbers", NULL, "character")
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    
    nSamples <- getConfigVariable("NumberOfSamples", 1000)
    subgroupSize <- getConfigVariable("SubgroupSize", 500)
    truncate <- getConfigVariable("TruncateToReference", TRUE)
    randomSeed <- getConfigVariable("RandomSeed", NULL, "integer")
    
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    refSession <- reference$getSourceSession()
    options <- reference$getTractOptions()

    if (!is.null(randomSeed))
        set.seed(randomSeed)

    model <- getNTResource("model", "pnt", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    nSessions <- length(sessionList)
    
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    if (results$nSessions() != nSessions)
        report(OL$Error, "Length of the session list specified does not match the results file")
    nPoints <- results$nPoints()

    searchWidth <- round(nPoints^(1/3))
    if (searchWidth^3 != nPoints)
        report(OL$Error, "Results file does not describe a cubic search space")
    
    if (is.null(sessionNumbers))
        sessionNumbers <- 1:nSessions
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    seedsInData <- all(c("x","y","z") %in% colnames(data))
    subjectsInData <- ("subject" %in% colnames(data)) && (is.integer(data$subject))
    
    parallelApply(sessionNumbers, function (i) {
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
            currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType=reference$getSeedUnit(), isStandard=TRUE)
        
        currentPosteriors <- results$getTractPosteriors(i)
        
        bestSeedIndex <- which.max(currentPosteriors)
        if (length(bestSeedIndex) == 0)
        {
            report(OL$Warning, "No match data available for session number ", i)
            return (invisible(NULL))
        }
        
        neighbourhood <- createNeighbourhoodInfo(searchWidth, centre=currentSeed)
        bestSeed <- neighbourhood$vectors[,bestSeedIndex]
        
        if (tracker == "tractor")
        {
            require("tractor.native")
            result <- trackWithSession(currentSession, bestSeed, nSamples=nSamples, requireImage=FALSE, requireStreamlines=TRUE)
            streamSet <- newStreamlineSetTractFromCollection(result$streamlines)
        }
        else
            streamSet <- newStreamlineSetTractFromProbtrack(currentSession, bestSeed, nSamples=nSamples)
        
        medianLine <- newStreamlineTractFromSet(streamSet, method="median", lengthQuantile=options$lengthQuantile, originAtSeed=TRUE)
        if (options$registerToReference)
        {
            if (is.null(refSession))
                transform <- newAffineTransform3DByInversion(getMniTransformForSession(currentSession))
            else
                transform <- newAffineTransform3DFromFlirt(currentSession$getImageFileNameByType("maskedb0"), refSession$getImageFileNameByType("maskedb0"))

            transformedMedianLine <- newStreamlineTractByTransformation(medianLine, transform)
        }
        medianSpline <- newBSplineTractFromStreamline(transformedMedianLine, knotSpacing=options$knotSpacing)
        medianData <- createDataTableForSplines(list(medianSpline), reference$getTract(), "knot")
        medianLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(medianData, model)
        report(OL$Info, "Median line log-likelihood is ", round(medianLogLikelihood,3))

        sessionData <- NULL
        nGroups <- (nSamples - 1) %/% subgroupSize + 1

        report(OL$Info, "Creating complete data table...")
        for (j in 1:nGroups)
        {
            firstStreamline <- subgroupSize * (j-1) + 1
            lastStreamline <- min(j*subgroupSize, nSamples)
            streamSubset <- newStreamlineSetTractBySubsetting(streamSet, firstStreamline:lastStreamline)
            splines <- calculateSplinesForStreamlineSetTract(streamSubset, currentSession, refSession, options)
            sessionData <- rbind(sessionData, createDataTableForSplines(splines,reference$getTract(),"knot"))
            rm(splines)
        }

        logLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(sessionData, model)
        logLikelihoodRatios <- logLikelihoods - medianLogLikelihood
        keepProbabilities <- exp(pmin(logLikelihoodRatios,0))
        toAccept <- which(!is.na(keepProbabilities) & runif(length(keepProbabilities)) <= keepProbabilities)
        toReject <- setdiff(1:nSamples, toAccept)
        
        if (length(toAccept) == 0)
        {
            report(OL$Warning, "All streamlines rejected for session ", i, " - using median line")
            lineMetadata <- newStreamlineTractMetadataFromImageMetadata(medianLine$getImageMetadata(), FALSE, "vox")
            medianLine <- newStreamlineTractWithMetadata(medianLine, lineMetadata)
            streamSet <- newStreamlineSetTractFromStreamline(medianLine)
        }
        else
        {
            report(OL$Info, "Rejecting ", length(toReject), " streamlines, of which ", sum(is.na(keepProbabilities)), " are missing")
            streamSet <- newStreamlineSetTractBySubsetting(streamSet, toAccept)
        }
        
        if (truncate)
        {
            report(OL$Info, "Truncating streamlines to the length of the reference tract")
            streamSet <- newStreamlineSetTractByTruncationToReference(streamSet, reference, currentSession)
        }
        
        report(OL$Info, "Creating visitation map")
        metadata <- newMriImageMetadataFromFile(currentSession$getImageFileNameByType("fa","diffusion"))
        visitationMap <- newMriImageAsVisitationMap(streamSet, metadata)
        fakeResult <- list(image=visitationMap, nSamples=streamSet$nStreamlines(), session=currentSession, seeds=promote(bestSeed,byrow=TRUE))
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeMriImageToFile(visitationMap, currentTractName)
        if (createImages)
            writePngsForResult(fakeResult, prefix=currentTractName, threshold=1/(streamSet$nStreamlines()+1), showSeed=showSeed)
    })
}
