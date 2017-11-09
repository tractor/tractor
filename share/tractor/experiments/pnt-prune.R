#@desc Visualise the results of evaluating a data set against a PNT model, creating an Analyze/NIfTI/MGH volume representing the best matching tract. A single seed point is used in each case, but individual streamlines generated from this seed are retained or rejected depending on their likelihood under the model. If a session directory is not specified then an image will be created for each session path stored in the dataset.
#@args [session directories]

library(tractor.reg)
library(tractor.track)
library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    nStreamlines <- getConfigVariable("Streamlines", 1000)
    subgroupSize <- getConfigVariable("SubgroupSize", 500)
    truncate <- getConfigVariable("TruncateToReference", TRUE)
    randomSeed <- getConfigVariable("RandomSeed", NULL, "integer")
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    refSession <- reference$getSourceSession()
    options <- reference$getTractOptions()
    model <- getNTResource("model", "pnt", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    
    if (!is.null(randomSeed))
        set.seed(randomSeed)
    
    data <- readPntDataTable(datasetName)
    if (nArguments() > 0)
        sessionList <- matchPaths(Arguments, unique(data$sessionPath))
    else
    {
        sessionList <- unique(data$sessionPath)
        attr(sessionList, "indices") <- 1:length(sessionList)
    }
    
    referenceSteps <- calculateSplineStepVectors(reference$getTract(), reference$getTractOptions()$pointType)
    if (nrow(referenceSteps$right) >= 2)
        rightwardsVector <- referenceSteps$right[2,]
    else if (nrow(referenceSteps$left) >= 2)
        rightwardsVector <- (-referenceSteps$left[2,])
    else
        report(OL$Error, "The reference tract has no length on either side")
    
    for (i in seq_along(sessionList))
    {
        if (is.na(sessionList[i]))
        {
            report(OL$Warning, "Session path #{Arguments[i]} does not appear in the dataset")
            next
        }
        
        report(OL$Info, "Generating tract for session #{sessionList[i]}, with index #{attr(sessionList,'indices')[i]}")
        currentSession <- attachMriSession(sessionList[i])
        currentSessionIndex <- attr(sessionList,"indices")[i]
        currentData <- subset(data, sessionPath==sessionList[i])
        currentPosteriors <- results$getTractPosteriors(currentSessionIndex)
        
        if (length(currentPosteriors) != nrow(currentData))
            report(OL$Error, "Posterior vector length does not match the dataset")
        
        bestSeedIndex <- which.max(currentPosteriors)
        if (length(bestSeedIndex) == 0)
        {
            report(OL$Warning, "No match data available for session number #{i}")
            return (invisible(NULL))
        }
        bestSeed <- as.matrix(currentData[bestSeedIndex,c("x","y","z")])
        report(OL$Info, "Best seed point is (#{implode(bestSeed,',')})")
        
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
            streamSource <- StreamlineSource$new(streamSource$select(NULL)$getMedian(options$lengthQuantile, pathOnly=TRUE))
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
        writeImageFile(visitationMap, es("#{tractName}.#{currentSessionIndex}"))
    }
}
