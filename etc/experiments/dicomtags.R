library(tractor.base)

runExperiment <- function ()
{
    requireArguments("file name")
    descriptions <- getWithDefault("Descriptions", TRUE)
    metadata <- newDicomMetadataFromFile(Arguments[1])
    if (is.null(metadata))
        output(OL$Error, "File ", Arguments[1], " appears not to be a DICOM file or is unreadable")
    else
        print(metadata, descriptions=descriptions)
}
