#@desc Visualise the results of evaluating a data set against a PNT model, creating Analyze/NIfTI/MGH volumes (with CreateVolumes:true) and/or projection images (CreateImages:true) of the best matching tracts for each session. The number of seed points contributing to the final tract is affected by the options MaximumSeedPoints and MinimumPosterior. In the default case a single seed point will always be used, but with MaximumSeedPoints:5 MinimumPosterior:0.01, for example, then up to 5 seeds will contribute, if that many have matching posterior probabilities of at least 0.01.
#@args [session directories]

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
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    
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
        
        report(OL$Info, "Generating tract for session #{sessionList[i]}")
        currentSession <- attachMriSession(sessionList[i])
        currentData <- subset(data, sessionPath==sessionList[i])
        currentPosteriors <- results$getTractPosteriors(attr(sessionList,"indices")[i])
        
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
            trackerPath <- currentSession$getTracker()$run(seeds, nStreamlines, requireMap=FALSE, requireStreamlines=TRUE)
            streamSource <- StreamlineSource$new(trackerPath)
            
            data <- array(0, dim=metadata$getDimensions())
            for (i in 1:nValidSeeds)
            {
                firstStreamline <- nStreamlines * (i-1) + 1
                lastStreamline <- i * nStreamlines
                imageForSeed <- streamSource$select(firstStreamline:lastStreamline)$getVisitationMap(metadata)
                data <- data + imageForSeed$getData() * currentPosteriors[sequence[i]]
            }
            
            report(OL$Info, "Creating visitation map")
            normalisationFactor <- sum(currentPosteriors[sequence])
            resultImage <- asMriImage(data/normalisationFactor, metadata)
            writeImageFile(resultImage, es("#{tractName}.#{i}"))
        }
        else
            report(OL$Warning, "No seed points above threshold for session number #{i}")
    }
}
