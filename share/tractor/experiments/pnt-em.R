#@desc Run Expectation-Maximisation to fit a tract matching model and apply it to a dataset. This script is generally used instead of the "pnt-train" and "pnt-eval" combination. The TractName and DatasetName should match those given to the "pnt-data" script. AlphaPriorMean controls the shape of the prior distribution; 0 indicates no prior. For small datasets in particular, 1 is usually a good setting. For larger datasets the effect of this parameter will be small.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getConfigVariable("MaximumKnotCount", NULL, "integer")
    asymmetricModel <- getConfigVariable("AsymmetricModel", TRUE)
    alphaPriorMean <- getConfigVariable("AlphaPriorMean", 1, "numeric")
    nullPrior <- getConfigVariable("NullPrior", NULL, "numeric")
    resultsName <- getConfigVariable("ResultsName", "results")
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    if (alphaPriorMean == 0)
        alphaPriorMean <- NULL
    
    # This is certainly the most sensible choice, so for now we hard-code it
    alphaOffset <- 1

    data <- readPntDataTable(datasetName)
    
    if (is.null(alphaPriorMean))
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, nullPrior=nullPrior, asymmetricModel=asymmetricModel)
    else
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, lambda=1/alphaPriorMean, alphaOffset=alphaOffset, nullPrior=nullPrior, asymmetricModel=asymmetricModel)
    
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, results$um)
    writeNTResource(resultsObject, "results", "pnt", list(resultsName=resultsName))
    
    model <- results$mm
    writeNTResource(model, "model", "pnt", list(tractName=tractName,datasetName=datasetName))

    print(model)
    
    # This test is redundant if a prior was used with alphaOffset >= 1
    if (is.null(alphaPriorMean))
    {
        alphas <- model$getAlphas()
        subUnityCount <- sum(alphas < 1, na.rm=TRUE)
        if (subUnityCount >= (length(alphas)/2))
            report(OL$Warning, "Half or more of the alphas are less than one - something is probably wrong")
        else if (subUnityCount > 0)
            report(OL$Warning, subUnityCount, " alpha value(s) less than one - the reference tract is a poor guide in these regions")
    }
}
