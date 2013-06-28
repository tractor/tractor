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
    else
        return (paste(session$getDirectory(), space, sep=":"))
}

transformImageToSpace <- function (image, session, newSpace, oldSpace = NULL, preferAffine = FALSE, reverseRegister = FALSE, finalInterpolation = 1, ...)
{
    require("tractor.reg")
    
    guessSpace <- function ()
    {
        if (image$isInternal())
            return (NULL)
        else if (dirname(image$getSource()) == .StandardBrainPath)
            return ("mni")
        else
        {
            for (space in setdiff(names(.RegistrationTargets),"mni"))
            {
                if (dirname(image$getSource()) == session$getDirectory(space))
                    return (space)
            }
        }
        
        return (NULL)
    }
    
    if (is.null(oldSpace))
        oldSpace <- guessSpace()
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    if (reverseRegister)
        transform <- session$getTransformation(.resolveSpace(newSpace,session), .resolveSpace(oldSpace,session), ...)
    else
        transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session), ...)
    
    newImage <- transformImage(transform, image, preferAffine=preferAffine, reverse=reverseRegister, finalInterpolation=finalInterpolation)
    
    return (newImage)
}

transformPointsToSpace <- function (points, session, newSpace, oldSpace = NULL, pointType = NULL, outputVoxel = FALSE, preferAffine = FALSE, reverseRegister = FALSE, nearest = FALSE, ...)
{
    require("tractor.reg")
    
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
    
    if (reverseRegister)
        transform <- session$getTransformation(.resolveSpace(newSpace,session), .resolveSpace(oldSpace,session), ...)
    else
        transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session), ...)
    
    if (outputVoxel && pointType == "mm")
    {
        if (reverseRegister)
            points <- changePointType(points, transform$getTargetImage(), "r", "mm")
        else
            points <- changePointType(points, transform$getSourceImage(), "r", "mm")
        pointType <- "r"
    }
    
    newPoints <- transformPoints(transform, points, voxel=(pointType!="mm"), preferAffine=preferAffine, reverse=reverseRegister, nearest=nearest)
    
    attr(newPoints, "space") <- .constructSpace(newSpace, session)
    attr(newPoints, "pointType") <- ifelse(pointType=="mm", "mm", "r")
    
    return (newPoints)
}

changePointType <- function (points, image, newPointType, oldPointType = NULL)
{
    require("tractor.reg")
    
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
        newPoints <- transformWorldToVoxel(points, image) - offsets[[newPointType]]
    else if (oldPointType != "mm" && newPointType == "mm")
        newPoints <- transformVoxelToWorld(points + offsets[[oldPointType]], image)
    else
        newPoints <- points + offsets[[oldPointType]] - offsets[[newPointType]]
    
    attr(newPoints, "pointType") <- ifelse(newPointType=="vox", "r", newPointType)
    
    return (newPoints)
}

coregisterDataVolumesForSession <- function (session, type, reference = 1, useMask = FALSE, nLevels = 2, options = list(), ...)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    require("tractor.reg")
    
    if (is(reference, "MriImage"))
        targetImage <- reference
    else
        targetImage <- session$getImageByType("rawdata", type, volumes=reference)
    
    if (useMask)
        maskImage <- session$getImageByType("mask", type)
    else
        maskImage <- NULL
    
    sourceMetadata <- session$getImageByType("rawdata", type, metadataOnly=TRUE)
    if (sourceMetadata$getDimensionality() != 4)
        report(OL$Error, "The raw data image is not 4-dimensional")
    nVolumes <- sourceMetadata$getDimensions()[4]
    
    if (length(nLevels) != nVolumes)
        nLevels <- rep(nLevels, length.out=nVolumes)
    
    finalArray <- array(NA, dim=sourceMetadata$getDimensions())
    transforms <- vector("list", nVolumes)
    
    report(OL$Info, "Coregistering data to reference volume...")
    for (i in seq_len(nVolumes))
    {
        report(OL$Verbose, "Reading and registering volume ", i)
        volume <- session$getImageByType("rawdata", type, volumes=i)
        result <- registerImages(volume, targetImage, targetMask=maskImage, types="affine", cache="ignore", ..., linearOptions=c(list(nLevels=nLevels[i]),options))
        finalArray[,,,i] <- result$transformedImage$getData()
        transforms[[i]] <- result$transform
    }
    
    finalImage <- newMriImageWithData(finalArray, sourceMetadata)
    writeImageFile(finalImage, session$getImageFileNameByType("data",type))
    
    transform <- mergeTransformations(transforms, sourceMetadata)
    transform$serialise(file.path(session$getDirectory(type), "coreg_xfm.Rdata"))
    
    return (transform)
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
