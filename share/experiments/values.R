#@args image file, [mask file]
#@desc Print out the values of the specified image, optionally limiting the region of interest to the nonzero voxels in a mask in the same space.

runExperiment <- function ()
{
    requireArguments("image file")
    
    digits <- getWithDefault("SignificantDigits", 6L)
    
    image <- newMriImageFromFile(Arguments[1])
    if (nArguments() > 1)
    {
        mask <- newMriImageFromFile(Arguments[2])
        locs <- which(mask$getData() != 0, arr.ind=TRUE)
    }
    else
        locs <- which(!is.na(image$getData()), arr.ind=TRUE)
    
    values <- signif(image[locs], digits)
    cat(implode(values, sep="\n"))
    cat("\n")
}
