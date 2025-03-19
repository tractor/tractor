#@desc Visualise the results of evaluating a data set against a PNT model, creating an Analyze/NIfTI/MGH volume representing the best matching tract. The number of seed points contributing to the final tract is affected by the options MaximumSeedPoints and MinimumPosterior. In the default case a single seed point will always be used, but with MaximumSeedPoints:5 MinimumPosterior:0.01, for example, then up to 5 seeds will contribute, if that many have matching posterior probabilities of at least 0.01. If a session directory is not specified then an image will be created for each session path stored in the dataset.
#@args [session directories]
#@group Neighbourhood tractography

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))
suppressPackageStartupMessages(require(tractor.track))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    maxSeeds <- getConfigVariable("MaximumSeedPoints", 1, "integer")
    minPosterior <- getConfigVariable("MinimumPosterior", 0, "numeric")
    nStreamlines <- getConfigVariable("Streamlines", 1000, "integer")
    
    reference <- getNTResource("reference", list(tractName=tractName))
    results <- getNTResource("results", list(resultsName=resultsName))
    
    data <- readPntDataTable(datasetName)
    if (nArguments() > 0)
        sessionList <- matchPaths(Arguments, unique(data$sessionPath))
    else
    {
        sessionList <- unique(data$sessionPath)
        attr(sessionList, "indices") <- 1:length(sessionList)
    }
    
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
        
        ranks <- rank(currentPosteriors, na.last="keep")
        maxRank <- max(ranks, na.rm=TRUE)
        ranks <- maxRank + 1 - ranks
        
        if (all(is.na(ranks)))
        {
            report(OL$Warning, "No match data available for session number #{i}")
            return (invisible(NULL))
        }
        
        if (maxRank < maxSeeds)
            maxSeeds <- maxRank
        indices <- match(1:maxSeeds, ranks)
        currentPosteriors[-indices] <- NA
        
        validSeeds <- which(currentPosteriors >= minPosterior)
        nValidSeeds <- length(validSeeds)
        report(OL$Info, "#{nValidSeeds} seed point(s) have weights above the threshold of #{minPosterior}")        

        if (nValidSeeds > 0)
        {
            metadata <- currentSession$getImageByType("maskedb0", metadataOnly=TRUE)
            sequence <- match(sort(currentPosteriors[validSeeds]), currentPosteriors)
            seeds <- as.matrix(currentData[sequence,c("x","y","z")])
            tracker <- currentSession$getTracker()
            
            visitationData <- array(0, dim=metadata$getDimensions())
            for (j in 1:nValidSeeds)
            {
                streamSource <- generateStreamlines(tracker, seeds[j,], nStreamlines)
                visitationData <- visitationData + streamSource$getVisitationMap()$getData() * currentPosteriors[sequence[j]]
            }
            
            report(OL$Info, "Creating visitation map")
            normalisationFactor <- sum(currentPosteriors[sequence])
            resultImage <- asMriImage(visitationData/normalisationFactor, metadata)
            writeImageFile(resultImage, es("#{tractName}.#{currentSessionIndex}"))
        }
        else
            report(OL$Warning, "No seed points above threshold for session number #{i}")
    }
}
