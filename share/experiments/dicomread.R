#@args directory, [output image name]
#@desc Create an Analyze/NIfTI volume by combining data from a set of DICOM files stored in the specified directory. A 3D or 4D image will be created, as appropriate, using the specified output name (or the same name as the directory if the second argument is missing).

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("directory")
    
    if (nArguments() > 1)
        fileName <- Arguments[2]
    else
        fileName <- Arguments[1]
    
    info <- newMriImageFromDicomDirectory(Arguments[1])
    reportFlags()
    writeMriImageToFile(info$image, fileName)
    info$image$summarise()
}
