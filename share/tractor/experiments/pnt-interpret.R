#@desc Extract parameters of interest from PNT results. Mode can be "location", for the voxel location of the best seed point (using the R convention); "posterior", for posterior matching probability at that point; "ratio", for the log-ratio of likelihoods relative to the reference tract (an indicator of goodness-of-fit); or "null-posterior", for the posterior probability of no match. If a session directory is not specified then a value will be given for each session path stored in the dataset.
#@args [session directories]

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    resultsName <- getConfigVariable("ResultsName", NULL, "character", errorIfMissing=TRUE)
    originalMaxSeeds <- getConfigVariable("MaximumSeedPoints", 1, "integer")
    minPosterior <- getConfigVariable("MinimumPosterior", 0, "numeric")
    mode <- getConfigVariable("Mode", NULL, "character", errorIfMissing=TRUE, c("location","posterior","ratio","null-posterior"), errorIfInvalid=TRUE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    model <- getNTResource("model", "pnt", list(tractName=tractName,datasetName=datasetName,modelName=modelName))
    results <- getNTResource("results", "pnt", list(resultsName=resultsName))
    
    data <- readPntDataTable(datasetName)
    if (nArguments() > 0)
        sessionList <- matchPaths(Arguments, unique(data$sessionPath))
    else
    {
        sessionList <- unique(data$sessionPath)
        attr(sessionList, "indices") <- 1:length(sessionList)
    }
    
    logLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, model)
    
    refData <- createDataTableForSplines(list(reference$getTract()), reference$getTract(), reference$getTractOptions()$pointType)
    refLogLikelihood <- calculateMatchedLogLikelihoodsForDataTable(refData, model)
    
    for (i in seq_along(sessionList))
    {
        if (is.na(sessionList[i]))
        {
            report(OL$Warning, "Session path #{Arguments[i]} does not appear in the dataset")
            next
        }
        
        report(OL$Info, "Current session is ", sessionList[i])
        currentData <- subset(data, sessionPath==sessionList[i])
        currentSeed <- round(apply(currentData[,c("x","y","z")], 2, median))
        currentPosteriors <- results$getTractPosteriors(attr(sessionList,"indices")[i])
        
        report(OL$Info, "Neighbourhood centroid is (#{implode(currentSeed,sep=',')})")
        if (length(currentPosteriors) != nrow(currentData))
            report(OL$Error, "Posterior vector length does not match the dataset")
        
        if (mode == "null-posterior")
        {
            cat(paste(results$getNullPosterior(attr(sessionList,"indices")[i]), "\n", sep=""))
            next
        }
        
        ranks <- rank(currentPosteriors, na.last="keep")
        maxRank <- max(ranks, na.rm=TRUE)
        ranks <- maxRank + 1 - ranks
        
        if (all(is.na(ranks)))
        {
            report(OL$Warning, "No match data available for session number #{i}")
            next
        }
        
        if (maxRank < originalMaxSeeds)
            maxSeeds <- maxRank
        else
            maxSeeds <- originalMaxSeeds
        
        indices <- match(1:maxSeeds, ranks)
        currentPosteriors[-indices] <- NA
        indices <- intersect(indices, which(currentPosteriors>=minPosterior))
        seeds <- as.matrix(currentData[indices,c("x","y","z")])
        
        for (j in 1:originalMaxSeeds)
        {
            if (j > nrow(seeds))
                cat("NA\n")
            else
            {
                likelihoodIndex <- ((attr(sessionList,"indices")[i]-1)*length(currentPosteriors)) + indices[j]
                value <- switch(mode, location=implode(seeds[j,],sep=","),
                                      posterior=currentPosteriors[indices[j]],
                                      ratio=(logLikelihoods[likelihoodIndex]-refLogLikelihood))
                cat(paste(value, "\n", sep=""))
            }
        }
    }
}
