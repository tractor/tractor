#@args session directory, image file or point

runExperiment <- function ()
{
    requireArguments("session directory", "image file or point")
    
    sourceSpace <- getConfigVariable("SourceSpace", NULL, "character", validValues=c("MNI","diffusion","structural","functional","freesurfer"))
    targetSpace <- getConfigVariable("TargetSpace", NULL, "character", validValues=c("MNI","diffusion","structural","functional","freesurfer"))
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","spline"))
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("FSL","R","vox","mm"))
    nearest <- getConfigVariable("Nearest", FALSE)
    
    tlc <- function(x) { if (is.null(x)) NULL else tolower(x) }
        
    session <- attachMriSession(Arguments[1])
    if (imageFileExists(implode(Arguments[-1], sep=" ")))
    {
        image <- readImageFile(implode(Arguments[-1], sep=" "))
        interpolation <- switch(interpolation, nearestneighbour=0, trilinear=1, spline=3)
        newImage <- transformImageToSpace(image, session, tlc(targetSpace), tlc(sourceSpace), preferAffine=preferAffine, interpolation=interpolation)
        writeImageFile(newImage, paste(basename(image$getSource()),targetSpace,sep="_"))
    }
    else
    {
        point <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (is.null(pointType))
            report(OL$Error, "Point type must be specified if a point location is given")
        newPoint <- transformPointsToSpace(point, session, tlc(targetSpace), tlc(sourceSpace), pointType=tlc(pointType), outputVoxel=TRUE, preferAffine=preferAffine, nearest=nearest)
        cat(paste(implode(round(newPoint,3),sep=","), "\n", sep=""))
    }
}
