#@args image file, [output file]
#@desc Binarise an Analyze/NIfTI image, giving all voxels greater than zero the value of 1, and all others 0. If the ThresholdLevel option is set then the appropriate threshold is applied before binarising. If no output file is specified then the input file will be overwritten.

runExperiment <- function ()
{
    requireArguments("image file")
    
    baseThreshold <- getWithDefault("ThresholdLevel", NULL, "numeric")
    thresholdMode <- getWithDefault("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    
    image <- newMriImageFromFile(Arguments[1])
    
    if (!is.null(baseThreshold))
    {
        threshold <- baseThreshold * switch(thresholdMode, nothing=1, maximum=max(image,na.rm=TRUE), minimum=min(image,na.rm=TRUE))
        image <- newMriImageByThresholding(image, threshold)
    }
    
    image <- newMriImageWithSimpleFunction(image, function(x) ifelse(x>0, 1, 0), newDataType=getDataTypeByNiftiCode(2))
    
    writeMriImageToFile(image, Arguments[nArguments()])
    invisible(NULL)
}
