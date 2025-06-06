#@desc Transform a point or image between named spaces as they relate to a particular session. Registration will be performed implicitly if necessary. The PointType option allows "world" coordinates or FSL-style zero-based voxel indices to be converted to TractoR's one-based voxel indices. The latter is always used for output.
#@args session directory, image file or point
#@group Working with sessions

library(tractor.track)

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
    
    if (is.null(targetSpace))
        targetSpace <- sourceSpace
    
    session <- attachMriSession(Arguments[1])
    fileStem <- implode(Arguments[-1], sep=" ")
    if (imageFileExists(fileStem))
    {
        image <- readImageFile(implode(Arguments[-1], sep=" "))
        interpolation <- switch(interpolation, nearestneighbour=0, trilinear=1, spline=3)
        newImage <- transformImageToSpace(image, session, tlc(targetSpace), tlc(sourceSpace), preferAffine=preferAffine, interpolation=interpolation)
        writeImageFile(newImage, paste(basename(image$getSource()),targetSpace,sep="_"))
    }
    else if (any(file.exists(ensureFileSuffix(fileStem, c("trk","tck")))))
    {
        if (is.null(sourceSpace))
        {
            report(OL$Info, "Source space was not specified - assuming diffusion")
            sourceSpace <- "diffusion"
        }
        
        source <- readStreamlines(fileStem, readLabels=FALSE)
        xfm <- session$getTransformation(tlc(sourceSpace),tlc(targetSpace))$getTransformObjects(preferAffine=preferAffine)
        if (RNiftyReg::isImage(xfm))
            xfm <- RNiftyReg::deformationField(xfm, jacobian=FALSE)
        
        targetImage <- session$getRegistrationTarget(tlc(targetSpace))
        resultStem <- paste(ensureFileSuffix(basename(fileStem),NULL,strip=c("trk","tck")), targetSpace, sep="_")
        
        streamlines <- source$getStreamlines(simplify=FALSE)
        streamlines <- lapply(streamlines, function(x) {
            line <- RNiftyReg::applyTransform(xfm, x$getLine("vox"))
            return (asStreamline(line, x$getSeedIndex(), image=targetImage, coordUnit="vox"))
        })
        attachStreamlines(streamlines)$writeStreamlines(resultStem)
    }
    else
    {
        point <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (is.null(pointType))
            report(OL$Error, "Point type must be specified if a point location is given")
        
        if (targetSpace == sourceSpace)
        {
            newPoint <- changePointType(point, session$getRegistrationTarget(tlc(sourceSpace)), "r", pointType)
            if (nearest)
                newPoint <- round(newPoint)
        }
        else
            newPoint <- transformPointsToSpace(point, session, tlc(targetSpace), tlc(sourceSpace), pointType=tlc(pointType), outputVoxel=TRUE, preferAffine=preferAffine, nearest=nearest)
        
        cat(paste(implode(round(newPoint,3),sep=","), "\n", sep=""))
    }
}
