#@args reference image, other image(s)
#@group General analysis
#@nohistory TRUE

# To ensure that '@' shorthand is expanded properly
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("reference image", "other image(s)")
    
    minOverlap <- getConfigVariable("MinOverlap", 1)
    ignoreZeroes <- getConfigVariable("IgnoreZeroes", TRUE)
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
            locs <- union(reference$find(array=FALSE), image$find(array=FALSE))
            equal <- reference[locs] == image[locs]
        }
        else
            equal <- as.array(reference) == as.array(image)
        
        overlap <- sum(equal,na.rm=TRUE) / length(equal)
        if (overlap >= minOverlap)
            report(OL$Info, "Images #{Arguments[1]} and #{imagePath} match (#{overlap*100}% overlap)", round=2)
    }
}
