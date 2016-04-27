#@args image file, data value(s) to extract
#@desc Create a volume which is the same as the input volume, except that all voxels whose value is in the list of data values to extract are set to one, and all other voxels are given the value zero. The base name of the output files is specified with the RegionName option.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("image file", "data value(s) to extract")
    image <- readImageFile(Arguments[1])
    values <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    regionName <- getConfigVariable("RegionName", "region")
    
    selectionFunction <- function (x)
    {
        dims <- dim(x)
        data <- ifelse(x %in% values, 1, 0)
        dim(data) <- dims
        return (data)
    }
    
    writeImageFile(image$map(selectionFunction), regionName)
}
