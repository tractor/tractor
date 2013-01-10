#@args image file, voxel location

runExperiment <- function ()
{
    requireArguments("image file", "voxel location")
    
    digits <- getConfigVariable("SignificantDigits", 6L)
    
    image <- readImageFile(Arguments[1])
    loc <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    cat(paste("Value of image \"", basename(image$getSource()), "\" at location (", implode(loc,","), ") is ", signif(image$getDataAtPoint(loc),digits), "\n", sep=""))
}

