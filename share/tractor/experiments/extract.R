#@args image file, data value(s) to extract
#@desc Create a volume which is the same as the input volume, except that all voxels whose value is in the list of data values to extract are set to one, and all other voxels are given the value zero. The base name of the output files is specified with the RegionName option.
#@group General analysis

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file", "data value(s) to extract")
    values <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    
    exclusions <- getConfigVariable("Exclude", NULL, "character")
    regionName <- getConfigVariable("RegionName", "region")
    
    if (!is.null(exclusions))
        exclusions <- splitAndConvertString(exclusions, ",", fixed=TRUE)
    
    namedValues <- !isValidAs(values, "numeric")
    namedExclusions <- !is.null(exclusions) && !isValidAs(exclusions,"numeric")
    if (namedValues || namedExclusions)
    {
        imagePath <- identifyImageFileNames(Arguments[1])
        if (file.exists(ensureFileSuffix(imagePath$fileStem, "lut")))
        {
            parcellation <- tractor.reg::readParcellation(Arguments[1])
            image <- parcellation$image
        }
        else
            report(OL$Error, "Data values are not numeric and there is no image lookup table")
    }
    else
        image <- readImageFile(Arguments[1])
    
    if (namedValues)
        values <- tractor.reg::matchRegions(values, parcellation)
    else
        values <- as.numeric(values)
    
    if (namedExclusions)
        values <- setdiff(values, tractor.reg::matchRegions(exclusions,parcellation))
    else if (!is.null(exclusions))
        values <- setdiff(values, as.numeric(exclusions))
    
    if (length(values) == 0)
        report(OL$Error, "There are no remaining data values to extract")
    else
    {
        report(OL$Info, "Retaining values #{implode(round(values,2),', ',' and ')}")
        writeImageFile(image$map(function(x) ifelse(x %in% values, 1L, 0L)), regionName)
    }
}
