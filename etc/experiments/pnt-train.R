suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    datasetName <- getWithDefault("TrainingDatasetName", NULL, "character", errorIfMissing=TRUE)
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    load(refFileName)
    if (!exists("reference") || !isReferenceTract(reference))
        output(OL$Error, "The file specified does not seem to contain reference tract information")
    if (!isBSplineTract(reference))
        output(OL$Error, "The specified reference tract is not in the correct form")

    data <- read.table(ensureFileSuffix(datasetName,"txt"))
    model <- newMatchingTractModelFromDataTable(data, reference$getTract(), maxLength=maxKnotCount)
    
    modelFileName <- ensureFileSuffix(paste(tractName,"model",sep="_"), "Rdata")
    save(model, file=ensureFileSuffix(modelFileName,"Rdata"))
    model$summarise()
}
