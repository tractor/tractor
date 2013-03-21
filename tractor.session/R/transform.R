AffineTransform3D <- setRefClass("AffineTransform3D", contains="SerialisableObject", fields=list(matrix="matrix",voxelMatrix="matrix",type="character",sourceMetadata="MriImage",destMetadata="MriImage"), methods=list(
    initialize = function (...)
    {
        object <- initFields(...)
        
        if (!equivalent(dim(object$matrix), c(4,4)))
            report(OL$Error, "The transformation must be specified as a 4x4 matrix")
        if (!isTRUE(object$type %in% "flirt"))
            report(OL$Error, "The specified transformation type, ", object$type, ", is not supported")

        dimnames(object$matrix) <- NULL

        sourceVoxToWorld <- transformFslVoxelToWorld(NULL, object$sourceMetadata)
        destVoxToWorld <- transformFslVoxelToWorld(NULL, object$destMetadata)

        # An extra translation matrix to deal with the R vs. C indexing conflict
        if (object$type %in% "flirt")
            voxelIndexShift <- transformRVoxelToFslVoxel(NULL)
        else
            voxelIndexShift <- diag(4)

        # NB. "solve" with one argument gives the inverse matrix
        object$voxelMatrix <- solve(voxelIndexShift) %*% solve(destVoxToWorld) %*% object$matrix %*% sourceVoxToWorld %*% voxelIndexShift
        
        return (object)
    },
    
    getDestinationMetadata = function () { return (destMetadata) },
    
    getMatrix = function (useVoxels = FALSE)
    {
        if (useVoxels)
            return (voxelMatrix)
        else
            return (matrix)
    },
    
    getSourceMetadata = function () { return (sourceMetadata) },
    
    getTransformType = function () { return (type) }
))
    
decomposeAffineTransform3D <- function (transform)
{
    if (!is(transform, "AffineTransform3D"))
        report(OL$Error, "The specified transformation is not an AffineTransform3D object")
    if (transform$getTransformType() != "flirt")
        report(OL$Error, "The specified transformation is not a FLIRT matrix")
    
    # Full matrix is rotationX %*% rotationY %*% rotationZ %*% skew %*% scale
    matrix <- transform$getMatrix()
    submatrix <- matrix[1:3,1:3]
    sm <- list(x=submatrix[,1], y=submatrix[,2], z=submatrix[,3])
    xLength <- vectorLength(sm$x)
    yLength <- sqrt((sm$y %*% sm$y) - (sm$x %*% sm$y)^2 / xLength^2)
    xyProj <- (sm$x %*% sm$y) / (xLength * yLength)
    xNorm <- sm$x / xLength
    yNorm <- (sm$y / yLength) - (xyProj * xNorm)
    zLength <- sqrt((sm$z %*% sm$z) - (xNorm %*% sm$z)^2 - (yNorm %*% sm$z)^2)
    xzProj <- (xNorm %*% sm$z) / zLength
    yzProj <- (yNorm %*% sm$z) / zLength
    
    scales <- c(xLength, yLength, zLength)
    scaleMatrix <- diag(scales)
    skews <- c(xyProj, xzProj, yzProj)
    skewMatrix <- diag(3)
    skewMatrix[c(4,7,8)] <- skews
    translation <- matrix[1:3,4]
    
    rotationMatrix <- submatrix %*% solve(scaleMatrix) %*% solve(skewMatrix)
    pitchAngle <- asin(-rotationMatrix[1,3])
    if (cos(pitchAngle) < 1e-4)
    {
        # Degenerate case (Gimbal lock) - fix yaw angle at zero
        rollAngle <- atan2(-rotationMatrix[3,2], rotationMatrix[2,2])
        yawAngle <- 0
    }
    else
    {
        rollAngle <- atan2(rotationMatrix[2,3], rotationMatrix[3,3])
        yawAngle <- atan2(rotationMatrix[1,2], rotationMatrix[1,1])
    }
    angles <- c(rollAngle, pitchAngle, yawAngle)
    
    return (list(scaleMatrix=scaleMatrix, skewMatrix=skewMatrix, rotationMatrix=rotationMatrix, translation=translation, scales=scales, skews=skews, angles=angles))
}

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

checkFlirtCacheForTransform <- function (sourceFile, destFile)
{
    cacheIndexFile <- file.path(tempdir(), "flirt-cache", "index.txt")
    if (!file.exists(cacheIndexFile))
        return (invisible(NULL))
    
    sourceFile <- expandFileName(sourceFile)
    destFile <- expandFileName(destFile)
    cacheIndex <- read.table(cacheIndexFile, col.names=c("source","dest","file"))
    cacheEntry <- subset(cacheIndex, (cacheIndex$source==sourceFile & cacheIndex$dest==destFile))
    
    if (nrow(cacheEntry) != 1)
        return (invisible(NULL))
    
    report(OL$Info, "FLIRT cache hit - reusing transform")
    transformFile <- as.vector(cacheEntry$file)
    transform <- deserialiseReferenceObject(transformFile)
    invisible(transform)
}

updateFlirtCacheWithTransform <- function (transform, sourceFile, destFile)
{
    if (!is.null(checkFlirtCacheForTransform(sourceFile, destFile)))
        return (FALSE)
    
    # Not using thread-safe directory here, because all threads need to see the cache
    cacheDir <- file.path(tempdir(), "flirt-cache")
    cacheIndexFile <- file.path(cacheDir, "index.txt")
    transformFile <- ensureFileSuffix(tempfile("transform-",cacheDir), "Rdata")
    
    if (!file.exists(cacheDir))
        dir.create(cacheDir)
    if (!file.exists(cacheIndexFile))
        file.create(cacheIndexFile)
    
    transform$serialise(file=transformFile)
    cacheEntry <- data.frame(source=sourceFile, dest=destFile, file=transformFile)
    write.table(cacheEntry, cacheIndexFile, append=TRUE, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

newAffineTransform3DFromFlirt <- function (source, dest, outfile = NULL, refweight = NULL)
{
    getImageMetadataAndFileName <- function (input)
    {
        isTemporary <- FALSE
        
        if (is(input, "MriImage"))
        {
            if (input$isInternal() || !file.exists(input$getSource()))
            {
                fileName <- threadSafeTempFile()
                writeImageFile(input, fileName)
                isTemporary <- TRUE
            }
            else
                fileName <- input$getSource()
            
            metadata <- input
        }
        else if (is.character(input) && (length(input) == 1))
        {
            fileName <- input
            metadata <- newMriImageFromFile(fileName, metadataOnly=TRUE)
            if (!is.na(metadata$getStoredXformMatrix()[1,1]))
            {
                xform <- metadata$getStoredXformMatrix()
                diagonal <- diag(xform)[1:3]
                tolerance <- 1e-3 * max(abs(diagonal))
                if (any(diagonal*c(-1,1,1) < 0) || !equivalent(xform[1:3,1:3],diag(diagonal),tolerance=tolerance))
                    flag(OL$Warning, "NIfTI image is not stored in the LAS convention - problems may occur")
            }
        }
        else
            report(OL$Error, "Source and destination must be specified as file names or MriImage objects")
        
        return (list(metadata=metadata, fileName=fileName, isTemporary=isTemporary))
    }
    
    source <- getImageMetadataAndFileName(source)
    dest <- getImageMetadataAndFileName(dest)

    transform <- checkFlirtCacheForTransform(source$fileName, dest$fileName)
    if (is.null(transform))
    {
        if (is.null(outfile))
            outfileExpression <- NULL
        else
            outfileExpression <- paste(" -out", outfile, sep=" ")
        if (is.null(refweight))
            refweightExpression <- NULL
        else
        {
            refweight <- getImageMetadataAndFileName(refweight)
            refweightExpression <- paste(" -refweight", refweight$fileName, sep=" ")
        }
        
        matrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
        logFile <- ensureFileSuffix(threadSafeTempFile(), "log")
        
        report(OL$Info, "Registering 3D volumes together with FLIRT")
        
        paramString <- paste("-in ", source$fileName, " -ref ", dest$fileName, outfileExpression, " -omat ", matrixFile, " -bins 256 -cost corratio -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 12", refweightExpression, " -interp trilinear >", logFile, " 2>&1", sep="")
        execute("flirt", paramString, errorOnFail=TRUE)
        
        transformMatrix <- as.matrix(read.table(matrixFile))
        transform <- AffineTransform3D$new(matrix=transformMatrix, type="flirt", sourceMetadata=source$metadata, destMetadata=dest$metadata)
        
        if (!source$isTemporary && !dest$isTemporary)
            updateFlirtCacheWithTransform(transform, source$fileName, dest$fileName)
        
        # Tidy up
        if (source$isTemporary)
            unlink(source$fileName)
        if (dest$isTemporary)
            unlink(dest$fileName)
        if (!is.null(refweight) && refweight$isTemporary)
            unlink(refweight$fileName)
        unlink(matrixFile)
        unlink(logFile)
    }

    invisible (transform)
}

newAffineTransform3DByInversion <- function (transform)
{
    transform <- AffineTransform3D$new(matrix=solve(transform$getMatrix()), type=transform$getTransformType(), sourceMetadata=transform$getDestinationMetadata(), destMetadata=transform$getSourceMetadata())
    invisible (transform)
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
