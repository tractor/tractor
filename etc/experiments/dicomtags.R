library(tractor.base)

runExperiment <- function ()
{
    requireArguments(1)
    descriptions <- getWithDefault("Descriptions", TRUE)
    print(newDicomMetadataFromFile(Arguments[1]), descriptions=descriptions)
}
