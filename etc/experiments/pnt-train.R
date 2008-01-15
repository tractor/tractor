suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    refTractFile <- getWithDefault("ReferenceTractFile", NULL, "character", errorIfMissing=TRUE)
    dataFile <- getWithDefault("DatasetFile", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    modelName <- getWithDefault("ModelName", "model")
    
    if (!all(c("seed","spline","options") %in% load(refTractFile)))
        output(OL$Error, "The file specified does not seem to contain reference tract information")

    data <- read.table(dataFile)
    model <- newMatchingTractModelFromDataTable(data, spline, maxLength=maxKnotCount)
    
    save(model, file=ensureFileSuffix(modelName,"Rdata"))
    model$summarise()
}
