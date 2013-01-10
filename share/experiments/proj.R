#@args image file, projection axis, [output file name]
#@desc Create a 2D maximum intensity projection of the specified Analyze/NIfTI/MGH volume along the x, y or z axis. The output file name is by default the same as the input file name, but with a "png" extension.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("image file", "projection axis")
    image <- readImageFile(Arguments[1])
    
    axis <- suppressWarnings(as.numeric(Arguments[2]))
    if (is.na(axis))
        axis <- which(tolower(Arguments[2]) == c("x","y","z"))
    if (length(axis) != 1 || !(axis %in% 1:3))
        report(OL$Error, "Projection axis must be specified as a letter (x-z) or number (1-3)")
    
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            report(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    if (nArguments() > 2)
        outputFile <- Arguments[3]
    else
        outputFile <- image$getSource()

    projectionNames <- c("sagittal", "coronal", "axial")    
    createProjectionGraphic(image, axis, device="png", file=paste(outputFile,projectionNames[axis],sep="_"), windowLimits=windowLimits)
}
