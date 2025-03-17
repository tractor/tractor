#@args image file or DICOM directory
#@desc List the key metadata parameters of the specified Analyze, NIfTI or MGH image volume. If a directory is specified, the script will assume that it is composed of DICOM files and attempt to reconstruct an image from these files. (See "dicomread" if this is your primary purpose.)
#@group General analysis
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    fileName <- implode(Arguments, sep=" ")
    
    if (isTRUE(file.info(fileName)$isdir))
        print(readDicomDirectory(fileName)$image)
    else
        print(readImageFile(fileName, metadataOnly=TRUE))
}
