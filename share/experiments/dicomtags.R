#@args file name
#@desc Produce a full list of all the DICOM tags contained in the specified file.
#@desc Descriptions of the tag, rather than its DICOM group and element numbers, will
#@desc be given unless Descriptions:false is specified.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("file name")
    descriptions <- getWithDefault("Descriptions", TRUE)
    fileName <- implode(Arguments, sep=" ")
    metadata <- newDicomMetadataFromFile(fileName)
    if (is.null(metadata))
        output(OL$Error, "File ", fileName, " appears not to be a DICOM file or is unreadable")
    else
        print(metadata, descriptions=descriptions)
}
