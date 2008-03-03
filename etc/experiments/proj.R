#@args image file, projection axis, [output file name]
#@desc Create a 2D maximum intensity projection of the specified Analyze/NIfTI volume
#@desc along the x, y or z axis. The output file name is by default the same as the
#@desc input file name, but with a "png" extension.

suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("image file", "projection axis")
    image <- newMriImageFromFile(Arguments[1])
    
    axis <- suppressWarnings(as.numeric(Arguments[2]))
    if (is.na(axis))
        axis <- which(tolower(Arguments[2]) == c("x","y","z"))
    if (length(axis) != 1 || !(axis %in% 1:3))
        output(OL$Error, "Projection axis must be specified as a letter (x-z) or number (1-3)")
    
    if (nArguments() > 2)
        outputFile <- Arguments[3]
    else
        outputFile <- image$getSource()

    projectionNames <- c("sagittal", "coronal", "axial")    
    createProjectionGraphic(image, axis, device="png", file=paste(outputFile,projectionNames[axis],sep="_"))
}
