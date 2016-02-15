#@desc Visualise the results of evaluating a data set against a PNT model, creating Analyze/NIfTI/MGH volumes (with CreateVolumes:true) and/or projection images (CreateImages:true) of the best matching tracts for each session. A single seed point is used in each case, but individual streamlines generated from this seed are retained or rejected depending on their likelihood under the model.

library(tractor.reg)
library(tractor.track)
library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    sessionNumbers <- getConfigVariable("SessionNumbers", NULL, "character")
    
    nStreamlines <- getConfigVariable("Streamlines", 1000)
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
    
    referenceSteps <- calculateSplineStepVectors(reference$getTract(), reference$getTractOptions()$pointType)
    if (nrow(referenceSteps$right) >= 2)
        rightwardsVector <- referenceSteps$right[2,]
    else if (nrow(referenceSteps$left) >= 2)
        rightwardsVector <- (-referenceSteps$left[2,])
    else
        report(OL$Error, "The reference tract has no length on either side")
    
    if (is.null(sessionNumbers))
        sessionNumbers <- 1:nSessions
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    seedsInData <- all(c("x","y","z") %in% colnames(data))
    subjectsInData <- ("subject" %in% colnames(data)) && (is.integer(data$subject))
    
    parallelApply(sessionNumbers, function (i) {
        report(OL$Info, "Generating tract for session #{i}")
        
        currentSession <- attachMriSession(sessionList[i])
        if (seedsInData)
        {
            if (subjectsInData)
                currentData <- subset(data, subject==i)
            else
                currentData <- data[(((i-1)*nPoints)+1):(i*nPoints),]
            currentSeed <- round(apply(currentData[,c("x","y","z")], 2, median))
        }
        else
            currentSeed <- transformPointsToSpace(reference$getStandardSpaceSeedPoint(), currentSession, "diffusion", oldSpace="mni", pointType=reference$getSeedUnit(), outputVoxel=TRUE, nearest=TRUE)
        
        currentPosteriors <- results$getTractPosteriors(i)
        
        bestSeedIndex <- which.max(currentPosteriors)
        if (length(bestSeedIndex) == 0)
        {
            report(OL$Warning, "No match data available for session number ", i)
            return (invisible(NULL))
        }
        
        neighbourhood <- createNeighbourhoodInfo(searchWidth, centre=currentSeed)
        bestSeed <- neighbourhood$vectors[,bestSeedIndex]
        report(OL$Info, "Seed point is (#{implode(bestSeed,",")})")
        
        tracker <- currentSession$getTracker()
        tracker$setOptions(rightwardsVector=rightwardsVector)
        trackerPath <- tracker$run(bestSeed, nStreamlines, requireMap=FALSE, requireStreamlines=TRUE)
        streamSource <- StreamlineSource$new(trackerPath)
        
        originalMedianLine <- streamSource$getMedian(options$lengthQuantile)
        medianLine <- transformStreamlineWithOptions(options, originalMedianLine$copy(), currentSession, refSession)
        medianSpline <- newBSplineTractFromStreamline(medianLine, knotSpacing=options$knotSpacing)
        medianData <- createDataTableForSplines(list(medianSpline), reference$getTract(), "knot")
        medianLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(medianData, model)
        report(OL$Info, "Median line log-likelihood is #{medianLogLikelihood}", round=3)

        sessionData <- NULL
        nGroups <- (nStreamlines - 1) %/% subgroupSize + 1

        report(OL$Info, "Creating complete data table...")
        for (j in 1:nGroups)
        {
            firstStreamline <- subgroupSize * (j-1) + 1
            lastStreamline <- min(j*subgroupSize, nStreamlines)
            
            splines <- vector("list", lastStreamline-firstStreamline+1)
            for (k in firstStreamline:lastStreamline)
            {
                streamline <- transformStreamlineWithOptions(options, streamSource$select(k)$getStreamlines(), currentSession, refSession)
                splines[[k-firstStreamline+1]] <- newBSplineTractFromStreamline(streamline, knotSpacing=options$knotSpacing)
                
                if (k %% 50 == 0)
                    report(OL$Verbose, "Done #{k}")
            }
            
            sessionData <- rbind(sessionData, createDataTableForSplines(splines,reference$getTract(),"knot"))
            rm(splines)
        }

        logLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(sessionData, model)
        logLikelihoodRatios <- logLikelihoods - medianLogLikelihood
        keepProbabilities <- exp(pmin(logLikelihoodRatios,0))
        toAccept <- which(!is.na(keepProbabilities) & runif(length(keepProbabilities)) <= keepProbabilities)
        toReject <- setdiff(1:nStreamlines, toAccept)
        
        if (length(toAccept) == 0)
        {
            report(OL$Warning, "All streamlines rejected for session #{i} - using median line")
            streamSource <- StreamlineSource$new(streamSource$getMedian(options$lengthQuantile, pathOnly=TRUE))
        }
        else
        {
            report(OL$Info, "Rejecting #{length(toReject)} streamlines, of which #{sum(is.na(keepProbabilities))} are missing")
            streamSource$select(toAccept)
        }
        
        if (truncate)
        {
            # Transform the reference tract into native space, find its length
            # in this space, and use that to truncate the streamline set
            report(OL$Info, "Truncating streamlines to the length of the reference tract")
            refPoints <- getPointsForTract(reference$getTract(), reference$getTractOptions()$pointType)
            
            if (reference$getTractOptions()$registerToReference)
            {
                if (is.null(refSession))
                    transform <- currentSession$getTransformation("diffusion", "mni")
                else
                    transform <- registerImages(currentSession$getRegistrationTargetFileName("diffusion"), refSession$getRegistrationTargetFileName("diffusion"))
    
                refPoints$points <- transformPoints(transform, refPoints$points, voxel=FALSE, reverse=TRUE)
            }
            
            refSteps <- calculateStepVectors(refPoints$points, refPoints$seedPoint)
            refLeftLength <- sum(apply(refSteps$left,1,vectorLength), na.rm=TRUE)
            refRightLength <- sum(apply(refSteps$right,1,vectorLength), na.rm=TRUE)
            
            streamSource <- streamSource$extractAndTruncate(refLeftLength, refRightLength)
        }
        
        report(OL$Info, "Creating visitation map")
        faPath <- currentSession$getImageFileNameByType("FA", "diffusion")
        visitationMap <- streamSource$getVisitationMap(faPath)
        
        currentTractName <- paste(tractName, "_session", i, sep="")
        if (createVolumes)
            writeImageFile(visitationMap, currentTractName)
    })
}
