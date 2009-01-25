.AffineTransform3D <- function (.matrix, .type, .sourceMetadata, .destMetadata)
{
    if (!is.matrix(.matrix) || (dim(.matrix) != c(4,4)))
        output(OL$Error, "The transformation must be specified as a 4x4 matrix")
    if (!isMriImageMetadata(.sourceMetadata) || !isMriImageMetadata(.destMetadata))
        output(OL$Error, "Source and destination images must be specified as MriImage objects")
    if (!(.type %in% "flirt"))
        output(OL$Error, "The specified transformation type, ", .type, ", is not supported")
    
    dimnames(.matrix) <- NULL
    
    sourceVoxToWorld <- transformFslVoxelToWorld(NULL, .sourceMetadata)
    destVoxToWorld <- transformFslVoxelToWorld(NULL, .destMetadata)
    
    # An extra translation matrix to deal with the R vs. C indexing conflict
    if (.type %in% "flirt")
        voxelIndexShift <- transformRVoxelToFslVoxel(NULL)
    else
        voxelIndexShift <- diag(4)
    
    # NB. "solve" with one argument gives the inverse matrix
    .voxelMatrix <- solve(voxelIndexShift) %*% solve(destVoxToWorld) %*% .matrix %*% sourceVoxToWorld %*% voxelIndexShift
    
    rm(sourceVoxToWorld, destVoxToWorld, voxelIndexShift)
    
    self <- list(
        getDestinationMetadata = function () { return (.destMetadata) },
        
        getMatrix = function (useVoxels = FALSE)
        {
            if (useVoxels)
                return (.voxelMatrix)
            else
                return (.matrix)
        },
        
        getSourceMetadata = function () { return (.sourceMetadata) },
        
        getTransformType = function () { return (.type) }
    )
    
    class(self) <- c("transform.affine.3d", "list.object", "list")
    invisible (self)
}

isAffineTransform3D <- function (object)
{
    return ("transform.affine.3d" %in% class(object))
}

deserialiseAffineTransform3D <- function (file = NULL, object = NULL)
{
    if (is.null(object))
        object <- deserialiseListObject(file, raw=TRUE)
    
    if (isDeserialisable(object$sourceMetadata, "metadata.image.mri"))
        object$sourceMetadata <- deserialiseMriImageMetadata(object=object$sourceMetadata)
    else
        output(OL$Error, "Deserialised object contains no valid source metadata")

    if (isDeserialisable(object$destMetadata, "metadata.image.mri"))
        object$destMetadata <- deserialiseMriImageMetadata(object=object$destMetadata)
    else
        output(OL$Error, "Deserialised object contains no valid target metadata")
    
    transform <- deserialiseListObject(NULL, object, .AffineTransform3D)
    invisible (transform)
}

checkFlirtCacheForTransform <- function (sourceFile, destFile)
{
    cacheIndexFile <- file.path(tempdir(), "flirt-cache", "index.txt")
    if (!file.exists(cacheIndexFile))
        return (invisible(NULL))
    
    sourceFile <- expandFileName(sourceFile)
    destFile <- expandFileName(destFile)
    cacheIndex <- read.table(cacheIndexFile, col.names=c("source","dest","file"))
    cacheEntry <- subset(cacheIndex, (source==sourceFile & dest==destFile))
    
    if (nrow(cacheEntry) != 1)
        return (invisible(NULL))
    
    output(OL$Info, "FLIRT cache hit - reusing transform")
    transformFile <- as.vector(cacheEntry$file)
    transform <- deserialiseAffineTransform3D(transformFile)
    invisible(transform)
}

updateFlirtCacheWithTransform <- function (transform, sourceFile, destFile)
{
    if (!is.null(checkFlirtCacheForTransform(sourceFile, destFile)))
        return (FALSE)
        
    cacheDir <- file.path(tempdir(), "flirt-cache")
    cacheIndexFile <- file.path(cacheDir, "index.txt")
    transformFile <- paste(tempfile("transform-",cacheDir), "Rdata", sep=".")
    
    if (!file.exists(cacheDir))
        dir.create(cacheDir)
    if (!file.exists(cacheIndexFile))
        file.create(cacheIndexFile)
    
    serialiseListObject(transform, file=transformFile)
    cacheEntry <- data.frame(source=sourceFile, dest=destFile, file=transformFile)
    write.table(cacheEntry, cacheIndexFile, append=TRUE, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

newAffineTransform3DFromFlirt <- function (source, dest, outfile = NULL, refweight = NULL)
{
    getImageAndFileName <- function (input)
    {
        isTemporary <- FALSE
        
        if (isMriImage(input))
        {
            if (input$isInternal() || !file.exists(input$getSource()))
            {
                fileName <- tempfile()
                writeMriImageToFile(input, fileName)
                isTemporary <- TRUE
            }
            else
                fileName <- input$getSource()
            
            image <- input
        }
        else if (is.character(input) && (length(input) == 1))
        {
            fileName <- input
            image <- newMriImageFromFile(fileName)
        }
        else
            output(OL$Error, "Source and destination must be specified as file names or MriImage objects")
        
        return (list(image=image, fileName=fileName, isTemporary=isTemporary))
    }
    
    source <- getImageAndFileName(source)
    dest <- getImageAndFileName(dest)

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
            refweight <- getImageAndFileName(refweight)
            refweightExpression <- paste(" -refweight", refweight$fileName, sep=" ")
        }
        
        matrixFile <- paste(tempfile(), ".mat", sep="")
        logFile <- paste(tempfile(), ".log", sep="")
        
        output(OL$Info, "Registering 3D volumes together with FLIRT")
        
        paramString <- paste("-in ", source$fileName, " -ref ", dest$fileName, outfileExpression, " -omat ", matrixFile, " -bins 256 -cost corratio -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 12", refweightExpression, " -interp trilinear >", logFile, " 2>&1", sep="")
        execute("flirt", paramString, errorOnFail=TRUE)
        
        transformMatrix <- as.matrix(read.table(matrixFile))
        transform <- .AffineTransform3D(transformMatrix, "flirt", source$image$getMetadata(), dest$image$getMetadata())
        
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
    matrix <- solve(transform$getMatrix())
    type <- transform$getTransformType()
    sourceMetadata <- transform$getDestinationMetadata()
    destMetadata <- transform$getSourceMetadata()
    
    transform <- .AffineTransform3D(matrix, type, sourceMetadata, destMetadata)
    invisible (transform)
}

resampleImageToDimensions <- function (image, voxelDims = NULL, imageDims = NULL, origin = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        output(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    if (is.null(imageDims))
        imageDims <- round(image$getFieldOfView() / abs(voxelDims))
    
    tempFiles <- tempfile(rep("file",4))
    
    writeMriImageToFile(image, tempFiles[1])
    write.table(diag(4), tempFiles[2], row.names=FALSE, col.names=FALSE)
    
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata(), imageDims=imageDims, voxelDims=voxelDims, datatype=getDataTypeByNiftiCode(2), origin=ifelse(is.null(origin),NA,origin))
    if (is.null(origin))
    {
        origin <- transformWorldToRVoxel(transformRVoxelToWorld(image$getOrigin(), image$getMetadata()), metadata)
        metadata <- newMriImageMetadataFromTemplate(metadata, origin=origin)
    }
    targetImage <- newMriImageWithData(array(0,dim=imageDims), metadata)
    writeMriImageToFile(targetImage, tempFiles[3])
    
    paramString <- paste("-in", tempFiles[1], "-applyxfm -init", tempFiles[2], "-ref", tempFiles[3], "-out", tempFiles[4], "-paddingsize 0.0 -interp trilinear 2>&1", sep=" ")
    execute("flirt", paramString, errorOnFail=TRUE)
    
    resampledImage <- newMriImageFromFile(tempFiles[4])
    
    unlink(tempFiles[1])
    removeImageFilesWithName(tempFiles[2:4])
    
    invisible (resampledImage)
}

transformPointsWithAffine <- function (transform, x, y = NULL, z = NULL, useVoxels = FALSE)
{
    if (!isAffineTransform3D(transform))
        output(OL$Error, "The specified transformation is not an AffineTransform3D object")
    
    if (is.vector(x))
        m <- matrix(resolveVector(len=3, x, y, z), nrow=1)
    else
        m <- x
    
    if (!is.matrix(m) || (length(dim(m)) != 2))
        output(OL$Error, "Points for transformation must be specified as an n by 3 matrix or 3 component vector")

    dims <- dim(m)
    if (dims[2] != 3)
    {
        if (dims[1] == 3)
        {
            output(OL$Warning, "Matrix does not have 3 columns; transposing it")
            m <- t(m)
            dims <- dim(m)
        }
        else
            output(OL$Error, "Matrix does not have 3 columns")
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
        metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("t2"))
        point <- transformWorldToRVoxel(point, metadata, useOrigin=TRUE)
    }
    
    point <- (if (round) round(point) else point)
    return (point)
}

getMniTransformForSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    xfmFileName <- session$getObjectFileName("transformFromMni")
    if (file.exists(xfmFileName))
        inverseXfm <- deserialiseAffineTransform3D(xfmFileName)
    else
    {
        whiteMatterImage <- getStandardImage("white")
        rescaleFunction <- function(x) { return ((x / 10) + 1) }
        refWeightImage <- newMriImageWithSimpleFunction(whiteMatterImage, rescaleFunction)

        source <- session$getImageFileNameByType("t2")
        dest <- getFileNameForStandardImage("brain")
        xfm <- newAffineTransform3DFromFlirt(source, dest, refweight=refWeightImage)
        inverseXfm <- newAffineTransform3DByInversion(xfm)
        serialiseListObject(inverseXfm, file=xfmFileName)
    }
    
    invisible (inverseXfm)
}

transformStandardSpaceImage <- function (session, image, toStandard = FALSE)
{
    if (!isMriImage(image))
        output(OL$Error, "Specified image is not an MriImage object")
    
    if (toStandard)
    {
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        dest <- getFileNameForStandardImage("brain")
    }
    else
    {
        xfm <- getMniTransformForSession(session)
        dest <- session$getImageFileNameByType("t2")
    }
    
    xfmFile <- ensureFileSuffix(tempfile(), "mat")
    write.table(xfm$getMatrix(), xfmFile, row.names=FALSE, col.names=FALSE)
    output(OL$Info, "Transforming image ", ifelse(toStandard,"to","from"), " standard space")
    
    imageFiles <- tempfile(rep("image",2))
    writeMriImageToFile(image, imageFiles[1])
    
    paramString <- paste("-in", imageFiles[1], "-ref", dest, "-applyxfm -init", xfmFile, "-out", imageFiles[2], sep=" ")
    execute("flirt", paramString, silent=TRUE, errorOnFail=TRUE)
    
    finalImage <- newMriImageFromFile(imageFiles[2])
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
    output(OL$Info, "Transforming points ", ifelse(toStandard,"to","from"), " standard space")
    
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
