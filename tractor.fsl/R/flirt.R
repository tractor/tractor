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
    transform <- get(load(transformFile))
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
    
    save(transform, file=transformFile)
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

getMniTransformForSession <- function (session)
{
    xfmFile <- session$getObjectFileName("xfmMniToT2")
    if (file.exists(xfmFile))
        inverseXfm <- get(load(xfmFile))
    else
    {
        whiteMatterImage <- newMriImageFromFile(file.path(.StandardBrainPath, "avg152T1_white"))
        rescaleFunction <- function(x) { return ((x / 10) + 1) }
        refWeightImage <- newMriImageWithSimpleFunction(whiteMatterImage, rescaleFunction)

        source <- session$getImageFileNameByType("t2")
        dest <- file.path(.StandardBrainPath, "avg152T1_brain")
        xfm <- newAffineTransform3DFromFlirt(source, dest, refweight=refWeightImage)
        inverseXfm <- newAffineTransform3DByInversion(xfm)
        save(inverseXfm, file=xfmFile)
    }
    
    invisible (inverseXfm)
}

transformStandardSpaceImage <- function (session, image)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    if (!isMriImage(image))
        output(OL$Error, "Specified image is not an MriImage object")
    
    xfm <- getMniTransformForSession(session)
    xfmFile <- ensureFileSuffix(tempfile(), "mat")
    write.table(xfm$getMatrix(), xfmFile, row.names=FALSE, col.names=FALSE)
    output(OL$Info, "Transforming image from standard space")
    
    imageFiles <- tempfile(rep("image",2))
    writeMriImageToFile(image, imageFiles[1])
    
    paramString <- paste("-in", imageFiles[1], "-ref", file.path(.StandardBrainPath,"avg152T1_brain"), "-applyxfm -init", xfmFile, "-out", imageFiles[2], sep=" ")
    execute("flirt", paramString, silent=TRUE, errorOnFail=TRUE)
    
    finalImage <- newMriImageFromFile(imageFiles[2])
    removeImageFilesWithName(imageFiles[1])
    removeImageFilesWithName(imageFiles[2])
    unlink(xfmFile)
    
    invisible (finalImage)
}

transformStandardSpaceSeeds <- function (session, seeds, unit = c("vox","mm"), round = TRUE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    if (is.null(.StandardBrainPath))
        output(OL$Error, "No standard brain volumes were found in the FSL tree")
    
    xfm <- getMniTransformForSession(session)
    output(OL$Info, "Transforming points from standard space")
    
    unit <- match.arg(unit)
    if (unit == "vox")
        coords <- transformVoxelPointsWithAffine(xfm, seeds)
    else
    {
        coords <- transformWorldToRVoxel(seeds, xfm$getSourceMetadata(), useOrigin=TRUE)
        coords <- transformVoxelPointsWithAffine(xfm, coords)
    }
    
    coords <- (if (round) round(coords) else coords)
    return (coords)
}
