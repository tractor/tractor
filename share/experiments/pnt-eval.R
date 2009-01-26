#@desc Evaluate a data set against a model (which must already exist) and calculate
#@desc matching probabilities. The TractName and DatasetName should match those used
#@desc for "pnt-data". The output file name is controlled by the ResultsName option.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", "results")
    
    modelFileName <- ensureFileSuffix(paste(tractName,"model",sep="_"), "Rdata")
    model <- deserialiseMatchingTractModel(modelFileName)
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    
    results <- calculatePosteriorsForDataTable(data, model)
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, NULL)
    writeNTResource(resultsObject, "results", "pnt", list(resultsName=resultsName))
}
