#@desc Evaluate a data set against the specified model (which must already exist) and calculate matching probabilities. The specified DatasetName should match that used for "pnt-data". The output file name is controlled by the ResultsName option.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getWithDefault("ModelName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", "results")
    
    model <- getNTResource("model", "pnt", list(modelName=modelName))
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    
    results <- calculatePosteriorsForDataTable(data, model)
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, NULL)
    writeNTResource(resultsObject, "results", "pnt", list(resultsName=resultsName))
}
