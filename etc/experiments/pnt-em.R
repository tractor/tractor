#@desc Run Expectation-Maximisation to fit a tract matching model and apply it to a
#@desc dataset. This script is generally used instead of the "pnt-train" and "pnt-eval"
#@desc combination. The TractName and DatasetName should match those given to the
#@desc "pnt-data" script. AlphaPriorMean controls the shape of the prior distribution;
#@desc NULL indicates no prior. For small datasets in particular, 1 is usually a good
#@desc setting. For larger datasets the effect of this parameter will be small.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    alphaPriorMean <- getWithDefault("AlphaPriorMean", NULL)
    nullPrior <- getWithDefault("NullPrior", NULL, "numeric")
    resultsName <- getWithDefault("ResultsName", "results")
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    reference <- deserialiseReferenceTract(refFileName)
    if (!isBSplineTract(reference))
        output(OL$Error, "The specified reference tract is not in the correct form")
    
    # This is certainly the most sensible choice, so for now we hard-code it
    alphaOffset <- 1

    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    
    if (is.null(alphaPriorMean))
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, alphaOffset=alphaOffset, nullPrior=nullPrior)
    else
        results <- runMatchingEMForDataTable(data, reference$getTract(), lengthCutoff=maxKnotCount, lambda=1/alphaPriorMean, alphaOffset=alphaOffset, nullPrior=nullPrior)
    
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, results$um)
    serialiseListObject(resultsObject, file=ensureFileSuffix(resultsName,"Rdata"))
    
    model <- results$mm
    modelFileName <- ensureFileSuffix(paste(tractName,"model",sep="_"), "Rdata")
    serialiseListObject(model, file=ensureFileSuffix(modelFileName,"Rdata"))

    model$summarise()
    
    alphas <- model$getAlphas()
    subUnityCount <- sum(alphas < 1, na.rm=TRUE)
    if (subUnityCount >= (length(alphas)/2))
        output(OL$Warning, "Half or more of the alphas are less than one - something is probably wrong")
    else if (subUnityCount > 0)
        output(OL$Warning, subUnityCount, " alpha value(s) less than one - the reference tract is a poor guide in these regions")
}
