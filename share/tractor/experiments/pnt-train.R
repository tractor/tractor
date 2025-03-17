#@desc Create a PNT model for tract matching using maximum likelihood on the specified data set. The TractName and DatasetName options should match those passed to the "pnt-data" experiment. MaximumKnotCount affects the lengths of tract that the model can describe. By default this will be set to the largest length in the training data set, but in some cases a larger value may need to be given.
#@group Probabilistic neighbourhood tractography

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getConfigVariable("DatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getConfigVariable("MaximumKnotCount", NULL, "integer")
    asymmetricModel <- getConfigVariable("AsymmetricModel", TRUE)
    
    reference <- getNTResource("reference", list(tractName=tractName))

    data <- readPntDataTable(datasetName)
    model <- newMatchingTractModelFromDataTable(data, reference$getTract(), maxLength=maxKnotCount, asymmetric=asymmetricModel)
    
    writeNTResource(model, "model", list(tractName=tractName,datasetName=datasetName))
    print(model)
}
