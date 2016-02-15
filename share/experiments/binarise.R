#@args image file, [output file]
#@desc Binarise an Analyze/NIfTI/MGH image, giving all voxels greater than zero the value of 1, and all others 0. If the ThresholdLevel option is set then the appropriate threshold is applied before binarising. If no output file is specified then the input file will be overwritten.

runExperiment <- function ()
{
    requireArguments("image file")
    
    baseThreshold <- getConfigVariable("ThresholdLevel", NULL, "numeric")
    thresholdMode <- getConfigVariable("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    
    image <- readImageFile(Arguments[1])
    
    if (!is.null(baseThreshold))
    {
        threshold <- baseThreshold * switch(thresholdMode, nothing=1, maximum=max(image,na.rm=TRUE), minimum=min(image,na.rm=TRUE))
        image <- image$threshold(threshold)
    }
    
    writeImageFile(image$binarise(), Arguments[nArguments()])
    invisible(NULL)
}
