#@desc Run Expectation-Maximisation to fit a tract matching model and apply it to a
#@desc dataset. This script is generally used instead of the "pnt-train" and "pnt-eval"
#@desc combination. The TractName and DatasetName should match those given to the
#@desc "pnt-data" script. AlphaPriorMean controls the shape of the prior distribution;
#@desc NULL indicates no prior. For small datasets in particular, 1 is usually a good
#@desc setting. For larger datasets the effect of this parameter will be small.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    asymmetricModel <- getWithDefault("AsymmetricModel", FALSE)
    alphaPriorMean <- getWithDefault("AlphaPriorMean", NULL, "numeric")
    nullPrior <- getWithDefault("NullPrior", NULL, "numeric")
    resultsName <- getWithDefault("ResultsName", "results")
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    # This is certainly the most sensible choice, so for now we hard-code it
    alphaOffset <- 1

    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    
    if (is.null(alphaPriorMean))
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, nullPrior=nullPrior, asymmetricModel=asymmetricModel)
    else
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, lambda=1/alphaPriorMean, alphaOffset=alphaOffset, nullPrior=nullPrior, asymmetricModel=asymmetricModel)
    
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, results$um)
    writeNTResource(resultsObject, "results", "pnt", list(resultsName=resultsName))
    
    model <- results$mm
    writeNTResource(model, "model", "pnt", list(tractName=tractName,datasetName=datasetName))

    model$summarise()
    
    # This test is redundant if a prior was used with alphaOffset >= 1
    if (is.null(alphaPriorMean))
    {
        alphas <- model$getAlphas()
        subUnityCount <- sum(alphas < 1, na.rm=TRUE)
        if (subUnityCount >= (length(alphas)/2))
            output(OL$Warning, "Half or more of the alphas are less than one - something is probably wrong")
        else if (subUnityCount > 0)
            output(OL$Warning, subUnityCount, " alpha value(s) less than one - the reference tract is a poor guide in these regions")
    }
}
