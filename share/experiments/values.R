#@args image file, [mask file]
#@desc Print out the values of the specified image, optionally limiting the region of interest to the nonzero voxels in a mask in the same space.

runExperiment <- function ()
{
    requireArguments("image file")
    
    digits <- getConfigVariable("SignificantDigits", 6L)
    
    if (nArguments() > 1)
    {
        mask <- newMriImageFromFile(Arguments[2])
        locs <- which(mask$getData() != 0, arr.ind=TRUE)
    }
    else
        locs <- which(!is.na(image$getData()), arr.ind=TRUE)
    
    if (mask$getSparseness() > 0.8)
        image <- newMriImageFromFile(Arguments[1], sparse=TRUE, mask=mask)
    else
        image <- newMriImageFromFile(Arguments[1])
    
    values <- signif(image[locs], digits)
    cat(implode(values, sep="\n"))
    cat("\n")
}
