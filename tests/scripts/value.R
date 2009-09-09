#@args image file, voxel location

runExperiment <- function ()
{
    requireArguments("image file", "voxel location")
    
    image <- newMriImageFromFile(Arguments[1])
    loc <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    cat(paste("Value of image \"", basename(image$getSource()), "\" at location (", implode(loc,","), ") is ", image$getDataAtPoint(loc), "\n", sep=""))
}

