#@args image file, slice axis, [output file name]
#@desc Create a contact sheet graphic showing all the slices of the specified Analyze/NIfTI volume in a matrix of images. The slice axis should be "x" for sagittal slices, "y" for coronal and "z" for axial. The number of columns in the layout can be controlled with the Columns options, while the Clearance option sets the amount of space left around each slice in the graphic (in voxels).

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file", "slice axis")
    image <- newMriImageFromFile(Arguments[1])
    
    if (!isValidAs(Arguments[2], "integer"))
        axis <- which(tolower(Arguments[2]) == c("x","y","z"))
    else
        axis <- as.numeric(Arguments[2])
    
    if (length(axis) != 1 || !(axis %in% 1:3))
        report(OL$Error, "Slice axis must be specified as a letter (x-z) or number (1-3)")
    
    clearance <- getConfigVariable("Clearance", 4, "integer")
    nColumns <- getConfigVariable("Columns", NULL, "integer")
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

    createContactSheetGraphic(image, axis, device="png", file=outputFile, windowLimits=windowLimits, clearance=clearance, nColumns=nColumns)
}
