suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    refTractFile <- getWithDefault("ReferenceTractFile", NULL, "character", errorIfMissing=TRUE)
    modelFile <- getWithDefault("ModelFile", NULL, "character", errorIfMissing=TRUE)
    dataFile <- getWithDefault("DatasetFile", NULL, "character", errorIfMissing=TRUE)
    resultsName <- getWithDefault("ResultsName", "results")
    
    if (!all(c("seed","spline","options") %in% load(refTractFile)))
        output(OL$Error, "The file specified does not seem to contain reference tract information")

    load(modelFile)
    data <- read.table(dataFile)
    
    results <- calculatePosteriorsForDataTable(data, model)
    with(results, save(tp, np, mm, file=ensureFileSuffix(resultsName,"Rdata")))
}
