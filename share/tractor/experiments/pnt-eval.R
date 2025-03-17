#@desc Evaluate a data set against the specified model (which must already exist) and calculate matching probabilities. The specified DatasetName should match that used for "pnt-data". The output file name is controlled by the ResultsName option.
#@group Probabilistic neighbourhood tractography

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    tractName <- getConfigVariable("TractName", NULL, "character")
    resultsName <- getConfigVariable("ResultsName", "results")
    
    model <- getNTResource("model", list(modelName=modelName,tractName=tractName))
    
    data <- readPntDataTable(datasetName)
    
    results <- calculatePosteriorsForDataTable(data, model)
    resultsObject <- newProbabilisticNTResultsFromPosteriors(results$tp, results$np, results$mm, NULL)
    writeNTResource(resultsObject, "results", list(resultsName=resultsName))
}
