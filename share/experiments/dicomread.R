#@args directory, output image name
#@desc Create an Analyze/NIfTI volume by combining data from a set of DICOM files
#@desc stored in the specified directory. A 3D or 4D image will be created, as
#@desc appropriate, using the specified output name.

library(tractor.base)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("directory", "output image name")
    
    info <- newMriImageFromDicomDirectory(Arguments[1])
    reportFlags()
    writeMriImageToFile(info$image, Arguments[2])
    info$image$summarise()
}
