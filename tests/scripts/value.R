#@args image file, voxel location

runExperiment <- function ()
{
    requireArguments("image file", "voxel location")
    
    digits <- getConfigVariable("SignificantDigits", 6L)
    
    loc <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    if (Arguments[1] %~% "\\.png$")
    {
        image <- loder::readPng(Arguments[1])
        cat(paste("Value of image \"", basename(ensureFileSuffix(Arguments[1],NULL,strip="png")), "\" at location (", implode(loc,","), ") is ", signif(do.call("[",c(list(image),as.list(loc))),digits), "\n", sep=""))
    }
    else
    {
        image <- readImageFile(Arguments[1])
        cat(paste("Value of image \"", basename(image$getSource()), "\" at location (", implode(loc,","), ") is ", signif(image$getDataAtPoint(loc),digits), "\n", sep=""))
    }
}

