#@args image file, data value(s) to extract
#@desc Create a volume which is the same as the input volume, except that all voxels whose value is in the list of data values to extract are set to one, and all other voxels are given the value zero. The base name of the output files is specified with the RegionName option.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file", "data value(s) to extract")
    values <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    
    regionName <- getConfigVariable("RegionName", "region")
    
    if (isValidAs(values, "numeric"))
    {
        image <- readImageFile(Arguments[1])
        values <- as.numeric(values)
    }
    else
    {
        imagePath <- identifyImageFileNames(Arguments[1])
        if (file.exists(ensureFileSuffix(imagePath$fileStem, "lut")))
        {
            parcellation <- tractor.reg::readParcellation(Arguments[1])
            image <- parcellation$image
            values <- tractor.reg::matchRegions(values, parcellation)
        }
        else
            report(OL$Error, "Data values are not numeric and there is no image lookup table")
    }
    
    writeImageFile(image$map(function(x) ifelse(x %in% values, 1L, 0L)), regionName)
}
