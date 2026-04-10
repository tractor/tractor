#@desc Compare images for equivalence in dimensions and content. The second and subsequent files specified are compared against the first, and a message will report any matches. Only the first volume will be compared unless AllVolumes is set to true. If IgnoreZeroes is set then only voxels that are nonzero in both images will be compared; MinOverlap indicates the proportion of remaining voxels that must be equal for a match to be identified.
#@args reference image, other image(s)
#@group General analysis
#@nohistory TRUE

# To ensure that '@' shorthand is expanded properly
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("reference image", "other image(s)")
    
    minOverlap <- getConfigVariable("MinOverlap", 1)
    ignoreZeroes <- getConfigVariable("IgnoreZeroes", FALSE)
    allVolumes <- getConfigVariable("AllVolumes", FALSE)
    # tolerance <- getConfigVariable("Tolerance", signif(sqrt(.Machine$double.eps),3))
    
    if (allVolumes)
        volumes <- NULL
    else
        volumes <- 1L
    
    reference <- readImageFile(Arguments[1], volumes=volumes)
    for (imagePath in Arguments[-1])
    {
        image <- readImageFile(imagePath, volumes=volumes)
        if (!equivalent(dim(reference), dim(image)))
            next
        if (!equivalent(reference$getVoxelDimensions(), image$getVoxelDimensions()))
            next
        if (ignoreZeroes)
        {
            locs <- intersect(reference$find(array=FALSE), image$find(array=FALSE))
            equal <- reference[locs] == image[locs]
        }
        else
            equal <- as.array(reference) == as.array(image)
        
        overlap <- sum(equal,na.rm=TRUE) / length(equal)
        if (overlap >= minOverlap)
            report(OL$Info, "Images #{Arguments[1]} and #{imagePath} match (#{overlap*100}% overlap)", round=2)
    }
}
