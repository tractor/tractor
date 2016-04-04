.resolveSpace <- function (space, session)
{
    spacePieces <- unlist(strsplit(space, ":", fixed=TRUE))
    if (length(spacePieces) == 2)
    {
        if (spacePieces[1] != session$getDirectory())
            report(OL$Error, "The current space does not correspond to the specified session")
        else
            return (spacePieces[2])
    }
    else
        return (spacePieces[1])
}

.constructSpace <- function (space, session)
{
    if (tolower(space) == "mni")
        return ("mni")
    else if (space %~% ore(":",syntax="fixed"))
        return (space)
    else
        return (paste(session$getDirectory(), space, sep=":"))
}

.findTransformation <- function (image, session, newSpace, oldSpace = NULL)
{
    if (is.null(oldSpace))
        oldSpace <- guessSpace(image, session)
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session))
    
    return (transform)
}

guessSpace <- function (image, session = NULL)
{
    if (image$isInternal())
        return (NULL)
    else if (dirname(image$getSource()) == .StandardBrainPath)
        return ("mni")
    else
    {
        if (is.null(session))
        {
            if (image$getSource() %~% "^(.+)/tractor")
                session <- attachMriSession(ore.lastmatch()[,1])
            else
                report(OL$Error, "Image does not seem to be within a session directory - the session must be specified")
        }
        
        for (space in setdiff(names(.RegistrationTargets),"mni"))
        {
            if (dirname(image$getSource()) == session$getDirectory(space))
                return (es("#{session$getDirectory()}:#{space}"))
        }
    }
    
    return (NULL)
}

transformImageToSpace <- function (image, session, newSpace, oldSpace = NULL, preferAffine = FALSE, interpolation = 1)
{
    transform <- .findTransformation(image, session, newSpace, oldSpace)
    newImage <- tractor.reg::transformImage(transform, image, preferAffine=preferAffine, interpolation=interpolation)
    
    return (newImage)
}

transformParcellationToSpace <- function (parcellation, session, newSpace, oldSpace = "structural", threshold = 0.5, preferAffine = FALSE)
{
    transform <- .findTransformation(parcellation$image, session, newSpace, oldSpace)
    newParcellation <- tractor.reg::transformParcellation(transform, parcellation, threshold, preferAffine=preferAffine)
    
    return (newParcellation)
}

transformPointsToSpace <- function (points, session, newSpace, oldSpace = NULL, pointType = NULL, outputVoxel = FALSE, preferAffine = FALSE, nearest = FALSE)
{
    if (is.null(pointType))
    {
        pointType <- attr(points, "pointType")
        if (is.null(pointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    pointType <- match.arg(tolower(pointType), c("fsl","r","vox","mm"))
    if (pointType == "fsl")
        points <- points + 1
    
    if (is.null(oldSpace))
        oldSpace <- attr(points, "space")
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session))
    
    if (outputVoxel && pointType == "mm")
    {
        points <- changePointType(points, transform$getSourceImage(), "r", "mm")
        pointType <- "r"
    }
    
    newPoints <- tractor.reg::transformPoints(transform, points, voxel=(pointType!="mm"), preferAffine=preferAffine, nearest=nearest)
    
    attr(newPoints, "space") <- .constructSpace(newSpace, session)
    attr(newPoints, "pointType") <- ifelse(pointType=="mm", "mm", "r")
    
    return (newPoints)
}

changePointType <- function (points, image, newPointType, oldPointType = NULL)
{
    if (is.null(oldPointType))
    {
        oldPointType <- attr(points, "pointType")
        if (is.null(oldPointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    # NB: point types "r" and "vox" are equivalent
    oldPointType <- match.arg(tolower(oldPointType), c("fsl","r","vox","mm"))
    newPointType <- match.arg(tolower(newPointType), c("fsl","r","vox","mm"))
    
    offsets <- list(fsl=1, r=0, vox=0)
    
    if (oldPointType == newPointType)
        newPoints <- points
    else if (oldPointType == "mm" && newPointType != "mm")
        newPoints <- tractor.reg::transformWorldToVoxel(points, image) - offsets[[newPointType]]
    else if (oldPointType != "mm" && newPointType == "mm")
        newPoints <- tractor.reg::transformVoxelToWorld(points + offsets[[oldPointType]], image)
    else
        newPoints <- points + offsets[[oldPointType]] - offsets[[newPointType]]
    
    attr(newPoints, "pointType") <- ifelse(newPointType=="vox", "r", newPointType)
    
    return (newPoints)
}

coregisterDataVolumesForSession <- function (session, type, reference = 1, useMask = FALSE, nLevels = 2, method = c("niftyreg","fsl","none"), options = list(), ...)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    if (is(reference, "MriImage"))
        targetImage <- reference
    else
        targetImage <- session$getImageByType("rawdata", type, volumes=reference)
    
    sourceMetadata <- session$getImageByType("rawdata", type, metadataOnly=TRUE)
    if (sourceMetadata$getDimensionality() != 4)
        report(OL$Error, "The raw data image is not 4-dimensional")
    nVolumes <- sourceMetadata$getDimensions()[4]
    
    if (method == "none")
    {
        report(OL$Info, "Storing identity transforms")
        volume <- session$getImageByType("rawdata", type, volumes=1)
        transforms <- rep(list(tractor.reg::identityTransformation(volume,targetImage)), nVolumes)
        
        report(OL$Info, "Symlinking data volume")
        symlinkImageFiles(session$getImageFileNameByType("rawdata",type), session$getImageFileNameByType("data",type), overwrite=TRUE)
    }
    else
    {
        if (useMask)
            maskImage <- session$getImageByType("mask", type)
        else
            maskImage <- NULL
        
        if (method == "niftyreg")
        {
            report(OL$Info, "Coregistering data to reference volume...")
            result <- tractor.reg::registerImages(session$getImageFileNameByType("rawdata",type), targetImage, targetMask=maskImage, types="affine", cache="ignore", method="niftyreg", ..., linearOptions=c(list(nLevels=nLevels,sequentialInit=TRUE),options))
            transforms <- result$transform
            
            report(OL$Info, "Writing out transformed data")
            writeImageFile(result$transformedImage, session$getImageFileNameByType("data",type))
        }
        else
        {
            finalArray <- array(NA, dim=sourceMetadata$getDimensions())
            transforms <- vector("list", nVolumes)
        
            report(OL$Info, "Coregistering data to reference volume...")
            for (i in seq_len(nVolumes))
            {
                report(OL$Verbose, "Reading and registering volume ", i)
                volume <- session$getImageByType("rawdata", type, volumes=i)
                result <- tractor.reg::registerImages(volume, targetImage, targetMask=maskImage, types="affine", cache="ignore", method=method, ..., linearOptions=c(list(nLevels=nLevels),options))
                finalArray[,,,i] <- result$transformedImage$getData()
                transforms[[i]] <- result$transform
            }
        
            report(OL$Info, "Writing out transformed data")
            finalImage <- asMriImage(finalArray, sourceMetadata)
            writeImageFile(finalImage, session$getImageFileNameByType("data",type))
        }
    }
    
    if (!is(transforms, "Transformation"))
        transforms <- tractor.reg::mergeTransformations(transforms, sourceMetadata)
    transforms$serialise(file.path(session$getDirectory(type), "coreg_xfm.Rdata"))
    
    return (transforms)
}

getVolumeTransformationForSession <- function (session, type)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    directory <- session$getDirectory(type)
    transformFileName <- file.path(directory, "coreg_xfm.Rdata")
    
    if (file.exists(transformFileName))
        return (deserialiseReferenceObject(transformFileName))
    else if (type == "diffusion")
        return (readEddyCorrectTransformsForSession(session))
    else
        report(OL$Error, "Transformation file does not exist for ", type, " data")
}
