#@args destination, directions file, b-values
#@desc Read diffusion gradient directions from a text file, and update the specified session directory. The text file should contain the diffusion gradient vectors applied to the data set, given either one-per-column or one-per-row, normalised or unnormalised, and with or without zeroes for b=0 measurements. If exactly one b-value is given then zero is assumed to be used in addition; otherwise all b-values must be given. A file containing the b-values for each volume in order may also be given as the third argument.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("destination", "directions file", "b-values")
    
    # True if destination exists and is a directory - assume this is a session
    if (isTRUE(file.info(Arguments["destination"])$isdir))
        destination <- attachMriSession(Arguments["destination"])$getImageFileNameByType("rawdata", "diffusion")
    # If the target matches an image, we will adopt a standard suffix
    else if (imageFileExists(destinationStem))
        destination <- Arguments["destination"]
    # Otherwise take the destination path literally, with whatever suffix it has
    else
        destination <- I(Arguments["destination"])
    
    if (nArguments() == 3 && file.exists(Arguments["b-values"]))
        bValues <- Arguments["b-values"]
    else
    {
        bValues <- sort(as.numeric(Arguments[-(1:2)]))
        if (length(bValues) == 1)
            bValues <- c(0, bValues)
    }
    
    scheme <- readDiffusionScheme(Arguments["directions file"], bValues, ensureFileSuffix(destination,NULL))
    
    report(OL$Info, "Writing gradient direction files")
    scheme$writeToFile(destination)
    
    print(scheme)
}
