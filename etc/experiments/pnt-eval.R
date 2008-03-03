#@desc Evaluate a data set against a model (which must already exist) and calculate
#@desc matching probabilities. The TractName and DatasetName should match those used
#@desc for "pnt-data". The output file name is controlled by the ResultsName option.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", "results")
    
    modelFileName <- ensureFileSuffix(paste(tractName,"model",sep="_"), "Rdata")
    load(modelFileName)
    if (!exists("model") || !isMatchingTractModel(model))
        output(OL$Error, "The file specified does not seem to contain a valid model")
    
    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    
    results <- calculatePosteriorsForDataTable(data, model)
    with(results, save(tp, np, mm, file=ensureFileSuffix(resultsName,"Rdata")))
}
