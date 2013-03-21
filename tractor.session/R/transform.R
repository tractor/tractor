readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
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
    
    imageMetadata <- session$getImageByType("refb0", metadataOnly=TRUE)
    transforms <- lapply(index, function (i) { AffineTransform3D$new(matrix=matrices[(((i-1)*4)+1):(i*4),], type="flirt", sourceMetadata=imageMetadata, destMetadata=imageMetadata) })
    
    invisible (transforms)
}

resampleImageToDimensions <- function (image, voxelDims = NULL, imageDims = NULL, origin = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        report(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    if (is.null(imageDims))
        imageDims <- round(image$getFieldOfView() / abs(voxelDims))
    
    tempFiles <- threadSafeTempFile(rep("file",4))
    
    writeImageFile(image, tempFiles[1])
    write.table(diag(4), tempFiles[2], row.names=FALSE, col.names=FALSE)
    
    targetImage <- newMriImageWithData(array(0,dim=imageDims), image, imageDims=imageDims, voxelDims=voxelDims)
    if (is.null(origin))
        origin <- transformWorldToRVoxel(transformRVoxelToWorld(image$getOrigin(), image), targetImage)
    targetImage$setSource(origin)
    writeImageFile(targetImage, tempFiles[3])
    
    paramString <- paste("-in", tempFiles[1], "-applyxfm -init", tempFiles[2], "-ref", tempFiles[3], "-out", tempFiles[4], "-paddingsize 0.0 -interp trilinear 2>&1", sep=" ")
    execute("flirt", paramString, errorOnFail=TRUE)
    
    resampledImage <- readImageFile(tempFiles[4])
    
    unlink(tempFiles[1])
    removeImageFilesWithName(tempFiles[2:4])
    
    invisible (resampledImage)
}

transformPointsWithAffine <- function (transform, x, y = NULL, z = NULL, useVoxels = FALSE)
{
    if (!is(transform, "AffineTransform3D"))
        report(OL$Error, "The specified transformation is not an AffineTransform3D object")
    
    if (is.vector(x))
        m <- matrix(resolveVector(len=3, x, y, z), nrow=1)
    else
        m <- x
    
    if (!is.matrix(m) || (length(dim(m)) != 2))
        report(OL$Error, "Points for transformation must be specified as an n by 3 matrix or 3 component vector")

    dims <- dim(m)
    if (dims[2] != 3)
    {
        if (dims[1] == 3)
        {
            flag(OL$Warning, "Matrix does not have 3 columns; transposing it")
            m <- t(m)
            dims <- dim(m)
        }
        else
            report(OL$Error, "Matrix does not have 3 columns")
    }
    
    inputMatrix <- cbind(m, rep(1,dims[1]))
    outputMatrix <- transform$getMatrix(useVoxels=useVoxels) %*% t(inputMatrix)
    
    return (drop(t(outputMatrix[1:3,])))
}

transformVoxelPointsWithAffine <- function (transform, x, y = NULL, z = NULL)
{
    return (transformPointsWithAffine(transform, x, y, z, useVoxels=TRUE))
}

transformWorldPointsWithAffine <- function (transform, x, y = NULL, z = NULL)
{
    return (transformPointsWithAffine(transform, x, y, z, useVoxels=FALSE))
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
