#@desc Evaluate a data set against the specified model (which must already exist) and calculate matching probabilities. The specified DatasetName should match that used for "pnt-data". The output file name is controlled by the ResultsName option.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getConfigVariable("ResultsName", "results")
    
    model <- getNTResource("model", "pnt", list(modelName=modelName))
    
    data <- readPntDataTable(datasetName)
    
    results <- calculatePosteriorsForDataTable(data, model)
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, NULL)
    writeNTResource(resultsObject, "results", "pnt", list(resultsName=resultsName))
}
