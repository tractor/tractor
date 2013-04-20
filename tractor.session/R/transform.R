readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    require("tractor.reg")
    
    logFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (!file.exists(logFile))
        report(OL$Error, "Eddy current correction log not found")
    logLines <- readLines(logFile)
    logLines <- subset(logLines, logLines %~% "^[0-9\\-\\. ]+$")
    
    connection <- textConnection(logLines)
    matrices <- as.matrix(read.table(connection))
    close(connection)
    
    if (is.null(index))
        index <- seq_len(nrow(matrices) / 4)
    
    matrices <- lapply(index, function(i) matrices[(((i-1)*4)+1):(i*4),])
    
    image <- session$getImageByType("refb0", "diffusion")
    transform <- Transformation$new(sourceImage=image, targetImage=image, affineMatrices=matrices, controlPointImages=list(), reverseControlPointImages=list(), method="flirt")
    
    invisible (transform)
}

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

transformImageBetweenSpaces <- function (image, session, sourceSpace = NULL, targetSpace = NULL, preferAffine = FALSE, reverse = FALSE, finalInterpolation = 1, ...)
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
    
    if (reverse && is.null(targetSpace))
        targetSpace <- guessSpace()
    else if (!reverse && is.null(sourceSpace))
        sourceSpace <- guessSpace()
    
    if (is.null(sourceSpace) || is.null(targetSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(sourceSpace,session), .resolveSpace(targetSpace,session), ...)
    
    newImage <- transformImage(transform, image, preferAffine=preferAffine, reverse=reverse, finalInterpolation=finalInterpolation)
    
    return (newImage)
}

transformPointsBetweenSpaces <- function (points, session, sourceSpace = NULL, targetSpace = NULL, pointType = NULL, preferAffine = FALSE, reverse = FALSE, nearest = FALSE, ...)
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
    
    if (reverse && is.null(targetSpace))
        targetSpace <- attr(points, "space")
    else if (!reverse && is.null(sourceSpace))
        sourceSpace <- attr(points, "space")
    
    if (is.null(sourceSpace) || is.null(targetSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(sourceSpace,session), .resolveSpace(targetSpace,session), ...)
    
    newPoints <- transformPoints(transform, points, voxel=(pointType!="mm"), preferAffine=preferAffine, reverse=reverse, nearest=nearest)
    
    if (reverse)
        attr(newPoints, "space") <- .constructSpace(sourceSpace, session)
    else
        attr(newPoints, "space") <- .constructSpace(targetSpace, session)
    
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

getNativeSpacePointForSession <- function (session, point, pointType, isStandard, round = TRUE)
{
    # NB: point types "r" and "vox" are equivalent
    pointType <- match.arg(tolower(pointType), c("fsl","r","vox","mm"))
    
    if (pointType == "fsl")
        point <- transformFslVoxelToRVoxel(point)
    
    if (isStandard)
        point <- transformStandardSpacePoints(session, point, unit=ifelse(pointType=="mm","mm","vox"))
    else if (pointType == "mm")
    {
        metadata <- session$getImageByType("maskedb0", metadataOnly=TRUE)
        point <- transformWorldToRVoxel(point, metadata, useOrigin=TRUE)
    }
    
    point <- (if (round) round(point) else point)
    return (point)
}

getMniTransformForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    xfmFileName <- session$getObjectFileName("transformFromMni")
    if (file.exists(xfmFileName))
        inverseXfm <- deserialiseReferenceObject(xfmFileName)
    else
    {
        whiteMatterImage <- getStandardImage("white")
        rescaleFunction <- function(x) { return ((x / 10) + 1) }
        refWeightImage <- newMriImageWithSimpleFunction(whiteMatterImage, rescaleFunction)

        source <- session$getImageFileNameByType("maskedb0")
        dest <- getFileNameForStandardImage("brain")
        xfm <- newAffineTransform3DFromFlirt(source, dest, refweight=refWeightImage)
        inverseXfm <- newAffineTransform3DByInversion(xfm)
        inverseXfm$serialise(file=xfmFileName)
    }
    
    invisible (inverseXfm)
}

transformStandardSpaceImage <- function (session, image, toStandard = FALSE)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not an MriImage object")
    
    if (toStandard)
    {
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        dest <- getFileNameForStandardImage("brain")
    }
    else
    {
        xfm <- getMniTransformForSession(session)
        dest <- session$getImageFileNameByType("maskedb0")
    }
    
    xfmFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
    write.table(xfm$getMatrix(), xfmFile, row.names=FALSE, col.names=FALSE)
    report(OL$Info, "Transforming image ", ifelse(toStandard,"to","from"), " standard space")
    
    imageFiles <- threadSafeTempFile(rep("image",2))
    writeImageFile(image, imageFiles[1])
    
    paramString <- paste("-in", imageFiles[1], "-ref", dest, "-applyxfm -init", xfmFile, "-out", imageFiles[2], sep=" ")
    execute("flirt", paramString, silent=TRUE, errorOnFail=TRUE)
    
    finalImage <- readImageFile(imageFiles[2])
    removeImageFilesWithName(imageFiles[1])
    removeImageFilesWithName(imageFiles[2])
    unlink(xfmFile)
    
    invisible (finalImage)
}

transformStandardSpacePoints <- function (session, points, unit = c("vox","mm"), round = TRUE, toStandard = FALSE)
{
    if (toStandard)
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
    else
        xfm <- getMniTransformForSession(session)
    report(OL$Info, "Transforming points ", ifelse(toStandard,"to","from"), " standard space")
    
    unit <- match.arg(unit)
    if (unit == "vox")
        coords <- transformVoxelPointsWithAffine(xfm, points)
    else
    {
        coords <- transformWorldToRVoxel(points, xfm$getSourceMetadata(), useOrigin=TRUE)
        coords <- transformVoxelPointsWithAffine(xfm, coords)
    }
    
    coords <- (if (round) round(coords) else coords)
    return (coords)
}
