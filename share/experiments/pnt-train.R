#@desc Create a PNT model for tract matching using maximum likelihood on the specified
#@desc data set. The TractName and DatasetName options should match those passed to the
#@desc "pnt-data" experiment. MaximumKnotCount affects the lengths of tract that the
#@desc model can describe. By default this will be set to the largest length in the
#@desc training data set, but in some cases a larger value may need to be given.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getConfigVariable("MaximumKnotCount", NULL, "integer")
    asymmetricModel <- getConfigVariable("AsymmetricModel", FALSE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))

    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    model <- newMatchingTractModelFromDataTable(data, reference$getTract(), maxLength=maxKnotCount, asymmetric=asymmetricModel)
    
    writeNTResource(model, "model", "pnt", list(tractName=tractName,datasetName=datasetName))
    model$summarise()
}
