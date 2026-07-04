#@args image file, [mask file]
#@desc Print out the values of the specified image, optionally limiting the region of interest to the nonzero voxels in a mask in the same space.
#@group General analysis

runExperiment <- function ()
{
    requireArguments("image file")
    
    digits <- getConfigVariable("SignificantDigits", 6L)
    
    if (nArguments() > 1)
    {
        mask <- readImageFile(Arguments[2])
        sparse <- (mask$getSparseness() > 0.8)
        image <- readImageFile(Arguments[1], sparse=sparse, mask=mask)
        values <- image[mask$find()]
    }
    else
    {
        image <- readImageFile(Arguments[1])
        locs <- image$find(fx(!is.na(x) & x!=0))
        values <- image[locs]
    }
    
    cat(implode(signif(values,digits), sep="\n"))
    cat("\n")
}
