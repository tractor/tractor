#@desc Create a PNT model for tract matching using maximum likelihood on the specified
#@desc data set. The TractName and DatasetName options should match those passed to the
#@desc "pnt-data" experiment. MaximumKnotCount affects the lengths of tract that the
#@desc model can describe. By default this will be set to the largest length in the
#@desc training data set, but in some cases a larger value may need to be given.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))

    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    model <- newMatchingTractModelFromDataTable(data, reference$getTract(), maxLength=maxKnotCount)
    
    writeNTResource(model, "model", "pnt", list(tractName=tractName,datasetName=datasetName))
    model$summarise()
}
