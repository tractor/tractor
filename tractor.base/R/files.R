getParametersForFileType <- function (fileType = NA, format = NA, singleFile = NA, gzipped = NA, errorIfInvalid = TRUE)
{
    if (is.character(fileType))
        typeIndex <- which(.FileTypes$typeNames == toupper(fileType))
    else
        typeIndex <- which(.FileTypes$formatNames == format & .FileTypes$singleFile == singleFile & .FileTypes$gzipped == gzipped)
    
    if (length(typeIndex) != 1)
    {
        if (errorIfInvalid)
            report(OL$Error, "Specified file type information is incomplete or invalid")
        else
            return (NULL)
    }
    
    parameters <- list(name=.FileTypes$typeNames[typeIndex],
                       format=.FileTypes$formatNames[typeIndex],
                       singleFile=.FileTypes$singleFile[typeIndex],
                       gzipped=.FileTypes$gzipped[typeIndex],
                       headerSuffix=.FileTypes$headerSuffixes[typeIndex],
                       imageSuffix=.FileTypes$imageSuffixes[typeIndex])
    
    return (parameters)
}

identifyImageFileNames <- function (fileName, fileType = NULL, errorIfMissing = TRUE)
{
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    fileName <- expandFileName(fileName)
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    headersExist <- intersect(unique(.FileTypes$headerSuffixes), suffixes[exist])
    imagesExist <- intersect(unique(.FileTypes$imageSuffixes), suffixes[exist])
    
    if (length(headersExist) < 1 || length(imagesExist) < 1)
    {
        if (errorIfMissing)
            report(OL$Error, "Complete image file does not exist: ", fileName)
        else
            return (NULL)
    }
    if (length(headersExist) > 1 || length(imagesExist) > 1)
    {
        if (errorIfMissing)
            report(OL$Error, "Multiple compatible image files exist: ", fileName)
        else
            return (NULL)
    }
    
    typeIndices <- which(.FileTypes$headerSuffixes == headersExist &
                         .FileTypes$imageSuffixes == imagesExist)
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, headersExist)
    imageFile <- ensureFileSuffix(fileStem, imagesExist)
    
    # ANALYZE and NIFTI_PAIR file types use the same filename suffixes
    if (length(typeIndices) == 1)
        format <- .FileTypes$format[typeIndices]
    else if (!is.null(fileType))
        format <- (getParametersForFileType(fileType, errorIfInvalid=TRUE))$format
    else
        format <- ifelse(hasNiftiMagicString(headerFile), "Nifti", "Analyze")
    
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile, format=format, headerSuffix=headersExist, imageSuffix=imagesExist)
    return (fileNames)
}

imageFileExists <- function (fileName, fileType = NULL)
{
    return (sapply(fileName, function (file) {
        !is.null(identifyImageFileNames(file, fileType, errorIfMissing=FALSE))
    }))
}

removeImageFilesWithName <- function (fileName)
{
    fileName <- expandFileName(fileName)
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)    
    files <- ensureFileSuffix(fileName, suffixes)
    unlink(files)
}

symlinkImageFiles <- function (from, to, overwrite = FALSE, relative = TRUE)
{
    if (length(from) != length(to))
        report(OL$Error, "The number of source and target file names must match")
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    for (i in seq_along(from))
    {
        info <- identifyImageFileNames(from[i])
        currentSource <- unique(c(info$headerFile, info$imageFile))
        currentTarget <- unique(ensureFileSuffix(expandFileName(to[i]), c(info$headerSuffix,info$imageSuffix), strip=suffixes))
        
        if (overwrite && any(file.exists(currentTarget)))
            unlink(currentTarget)
        if (relative)
        {
            for (j in seq_along(currentSource))
                currentSource[j] <- relativePath(currentSource[j], currentTarget[j])
        }
        
        file.symlink(currentSource, currentTarget)
    }
}

copyImageFiles <- function (from, to, overwrite = FALSE, deleteOriginals = FALSE)
{
    if (length(from) != length(to))
        report(OL$Error, "The number of source and target file names must match")
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    for (i in seq_along(from))
    {
        info <- identifyImageFileNames(from[i])
        currentSource <- c(info$headerFile, info$imageFile)
        currentTarget <- ensureFileSuffix(expandFileName(to[i]), c(info$headerSuffix,info$imageSuffix), strip=suffixes)
        
        success <- file.copy(unique(currentSource), unique(currentTarget), overwrite=overwrite)
        
        if (all(success) && deleteOriginals)
            removeImageFilesWithName(from[i])
    }
}

readImageFile <- function (fileName, fileType = NULL, metadataOnly = FALSE, warnIfNotLas = FALSE)
{
    fileNames <- identifyImageFileNames(fileName, fileType)
    
    readFun <- switch(fileNames$format, Analyze=readAnalyze, Nifti=readNifti, Mgh=readMgh)
    info <- readFun(fileNames)
    
    datatype <- info$imageMetadata$datatype
    endian <- info$storageMetadata$endian
    dims <- info$imageMetadata$imageDims
    voxelDims <- info$imageMetadata$voxelDims
    nVoxels <- prod(dims)
    nDims <- length(dims)
    
    if (!metadataOnly)
    {
        connection <- gzfile(fileNames$imageFile, "rb")
        if (fileNames$imageFile == fileNames$headerFile)
            seek(connection, where=info$storageMetadata$dataOffset)

        voxels <- readBin(connection, what=datatype$type, n=nVoxels, size=datatype$size, signed=datatype$isSigned, endian=endian)
        data <- array(voxels, dim=dims)
        close(connection)

        slope <- info$storageMetadata$dataScalingSlope
        intercept <- info$storageMetadata$dataScalingIntercept
        if (slope != 0 && !equivalent(c(slope,intercept), 1:0))
            data <- data * slope + intercept
    }
    
    rotationMatrix <- info$storageMetadata$xformMatrix[1:3,1:3]
    absRotationMatrix <- abs(rotationMatrix)
    tolerance <- 1e-3 * max(abs(voxelDims))
    
    # The rotation matrix should have exactly one nonzero element per row and column
    if (!equivalent(rowSums(absRotationMatrix > tolerance), c(1,1,1)) || !equivalent(colSums(absRotationMatrix > tolerance), c(1,1,1)))
        report(OL$Error, "NIfTI xform matrix is rotated")
    else
    {
        dimPermutation <- apply(absRotationMatrix > tolerance, 1, which)
        if (nDims > 3)
            dimPermutation <- c(dimPermutation, 4:nDims)
        if (!identical(dimPermutation, seq_len(nDims)))
        {
            if (warnIfNotLas)
                flag(OL$Warning, "NIfTI image is not stored in the LAS convention - problems may occur")
            if (!metadataOnly)
                data <- aperm(data, dimPermutation)
            dims <- dims[dimPermutation]
            voxelDims <- voxelDims[dimPermutation]
        }
        
        # Fix signs of voxel dimensions to correspond to LAS
        voxelDims <- abs(voxelDims) * c(-1, rep(1,nDims-1))
        
        # Figure out which dimensions need to be flipped
        ordering <- round(colSums(rotationMatrix) / c(abs(voxelDims[1:min(3,nDims)]),rep(1,max(0,3-nDims))))
        ordering <- ordering * c(-1, 1, 1)
        
        if (warnIfNotLas && any(ordering < 0))
            flag(OL$Warning, "NIfTI image is not stored in the LAS convention - problems may occur")
        
        if (nDims == 2)
        {
            origin <- 1 - ordering[1:2] * round(info$storageMetadata$xformMatrix[1:2,4]/voxelDims[1:2],2)
            origin <- ifelse(ordering[1:2] == c(1,1), origin, dims-origin+1)
        }
        else
        {
            origin <- c(1 - ordering[1:3] * round(info$storageMetadata$xformMatrix[1:3,4]/voxelDims[1:3],2), rep(0,nDims-3))
            origin[1:3] <- ifelse(ordering[1:3] == c(1,1,1), origin[1:3], dims[1:3]-origin[1:3]+1)
        }
        
        if (!metadataOnly)
        {
            orderX <- (if (ordering[1] == 1) seq_len(dims[1]) else rev(seq_len(dims[1])))
            orderY <- (if (ordering[2] == 1) seq_len(dims[2]) else rev(seq_len(dims[2])))
            if (nDims > 2)
                orderZ <- (if (ordering[3] == 1) seq_len(dims[3]) else rev(seq_len(dims[3])))
            dimsToKeep <- setdiff(1:nDims, 1:3)

            if (nDims == 2)
                data <- data[orderX, orderY]
            else if (nDims == 3)
                data <- data[orderX, orderY, orderZ]
            else
                data <- array(apply(data, dimsToKeep, "[", orderX, orderY, orderZ), dim=dim(data))
        }
    }
    
    imageMetadata <- MriImageMetadata$new(imagedims=dims, voxdims=voxelDims, voxunit=info$imageMetadata$voxelUnit, source=info$imageMetadata$source, datatype=datatype, origin=origin, tags=info$imageMetadata$tags)
    
    if (metadataOnly)
        invisible (imageMetadata)
    else
    {
        image <- MriImage$new(data, imageMetadata)
        invisible (image)
    }
}

newMriImageMetadataFromFile <- function (fileName, fileType = NULL, warnIfNotLas = FALSE)
{
    invisible (readImageFile(fileName, fileType, metadataOnly=TRUE, warnIfNotLas=warnIfNotLas))
}

newMriImageFromFile <- function (fileName, fileType = NULL, warnIfNotLas = FALSE)
{
    invisible (readImageFile(fileName, fileType, metadataOnly=FALSE, warnIfNotLas=warnIfNotLas))
}

writeMriImageToFile <- function (image, fileName = NULL, fileType = NA, datatype = NULL, overwrite = TRUE)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    if (!is.null(fileName))
        fileName <- expandFileName(fileName)
    else if (image$isInternal())
        report(OL$Error, "This image has no associated file name; it must be specified")
    else
        fileName <- image$getSource()
    
    params <- getParametersForFileType(fileType, errorIfInvalid=FALSE)
    if (is.null(params))
        params <- getParametersForFileType(getOption("tractorFileType"), errorIfInvalid=FALSE)
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    
    if (overwrite)
        unlink(files[exist])
    else if (sum(exist) > 0)
        report(OL$Error, "File exists and cannot be overwritten")
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, params$headerSuffix)
    imageFile <- ensureFileSuffix(fileStem, params$imageSuffix)
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile)
    
    if (params$format == "Analyze")
        writeMriImageToAnalyze(image, fileNames, gzipped=params$gzipped, datatype=datatype)
    else if (params$format == "Nifti")
        writeMriImageToNifti(image, fileNames, gzipped=params$gzipped, datatype=datatype)
    else if (params$format == "Mgh")
        writeMriImageToMgh(image, fileNames, gzipped=params$gzipped, datatype=datatype)
    
    invisible (fileNames)
}
