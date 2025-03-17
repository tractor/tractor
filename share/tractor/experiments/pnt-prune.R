#@desc Visualise the results of evaluating a data set against a PNT model, creating an Analyze/NIfTI/MGH volume representing the best matching tract. A single seed point is used in each case, but individual streamlines generated from this seed are retained or rejected depending on their likelihood under the model. If a session directory is not specified then an image will be created for each session path stored in the dataset.
#@args [session directories]
#@group Probabilistic neighbourhood tractography

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
    requireMap <- getConfigVariable("RequireMap", TRUE)
    requireStreamlines <- getConfigVariable("RequirePaths", FALSE)
    
    reference <- getNTResource("reference", list(tractName=tractName))
    refSession <- reference$getSourceSession()
    options <- reference$getTractOptions()
    model <- getNTResource("model", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    results <- getNTResource("results", list(resultsName=resultsName))
    
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
        
        # We need a stable streamline set across multiple passes, so we write to file and then read back in
        streamSource <- generateStreamlines(currentSession$getTracker(), bestSeed, nStreamlines, rightwardsVector=rightwardsVector)
        streamSource <- readStreamlines(streamSource$writeStreamlines())
        
        originalMedianLine <- streamSource$filter(medianOnly=TRUE, medianLengthQuantile=options$lengthQuantile)$getStreamlines()
        medianLine <- transformStreamlineWithOptions(options, originalMedianLine$copy(), currentSession, refSession)
        medianSpline <- newBSplineTractFromStreamline(medianLine, knotSpacing=options$knotSpacing)
        medianData <- createDataTableForSplines(list(medianSpline), reference$getTract(), "knot")
        medianLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(medianData, model)
        report(OL$Info, "Median line log-likelihood is #{medianLogLikelihood}", round=3)

        sessionData <- NULL
        nGroups <- (nStreamlines - 1) %/% subgroupSize + 1
        
        report(OL$Info, "Creating complete data table...")
        for (j in seq_len(nGroups))
        {
            firstStreamline <- subgroupSize * (j-1) + 1
            lastStreamline <- min(j*subgroupSize, nStreamlines)
            
            splines <- lapply(streamSource$select(firstStreamline:lastStreamline)$getStreamlines(), function(streamline) {
                streamline <- transformStreamlineWithOptions(options, streamline, currentSession, refSession)
                newBSplineTractFromStreamline(streamline, knotSpacing=options$knotSpacing)
            })
            
            sessionData <- rbind(sessionData, createDataTableForSplines(splines,reference$getTract(),"knot"))
            report(OL$Verbose, "Done #{lastStreamline}")
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
            streamSource$select(NULL)$filter(medianOnly=TRUE, medianLengthQuantile=options$lengthQuantile)
        }
        else
        {
            report(OL$Info, "Rejecting #{length(toReject)} streamlines, of which #{sum(is.na(keepProbabilities))} are missing")
            streamSource$select(toAccept)
        }
        
        truncationPoints <- list(left=NULL, right=NULL)
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
            truncationPoints$left <- sum(apply(refSteps$left,1,vectorLength), na.rm=TRUE)
            truncationPoints$right <- sum(apply(refSteps$right,1,vectorLength), na.rm=TRUE)
        }
        
        report(OL$Info, "Creating outputs")
        outputStem <- es("#{tractName}.#{currentSessionIndex}")
        result <- streamSource$process(outputStem, requireStreamlines=requireStreamlines, requireMap=requireMap, truncate=truncationPoints)
        if (!is.null(result$map))
            writeImageFile(result$map, outputStem)
    }
}
