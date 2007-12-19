library(tractor.base)

runExperiment <- function ()
{
    requireArguments("file name")
    descriptions <- getWithDefault("Descriptions", TRUE)
    print(newDicomMetadataFromFile(Arguments[1]), descriptions=descriptions)
}
