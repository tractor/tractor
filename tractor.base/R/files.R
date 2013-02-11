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
        
        # Don't try to copy an image onto itself
        if (all(currentSource == currentTarget))
            next
        
        success <- file.copy(unique(currentSource), unique(currentTarget), overwrite=overwrite)
        
        if (all(success) && deleteOriginals)
            removeImageFilesWithName(from[i])
    }
}

chooseDataTypeForImage <- function (image, format)
{
    if (image$isEmpty())
        return (NULL)
    else if (image$isSparse())
        data <- image$getData()$getData()
    else
        data <- image$getData()
    
    # Get the available data types for the specified format
    datatypes <- get(paste(".",format,sep=""))$datatypes
    
    # If double-mode data can be represented as integers, convert it to save space
    # Note that this slows the function down
    rType <- storage.mode(data)
    if (rType == "double" && equivalent(as.double(data),as.integer(data)))
        rType <- "integer"
    
    isSigned <- (rType == "double" || min(data,na.rm=TRUE) < 0)
    
    if (rType == "double")
    {
        singleTypeExists <- sum(datatypes$rTypes == "double" & datatypes$sizes == 4) == 1
        doubleTypeExists <- sum(datatypes$rTypes == "double" & datatypes$sizes == 8) == 1
        if (!singleTypeExists && !doubleTypeExists)
            report(OL$Error, "Floating-point data cannot be stored using the specified file format")
        
        if (singleTypeExists && (isTRUE(getOption("tractorOutputPrecision") == "single") || !doubleTypeExists))
            size <- 4
        else
            size <- 8
        
        isSigned <- TRUE
        code <- datatypes$codes[datatypes$rTypes == "double" & datatypes$sizes == size]
    }
    else
    {
        compatible <- (datatypes$rTypes == "integer")
        if (min(data,na.rm=TRUE) < 0)
            compatible <- compatible & datatypes$isSigned
        
        maximumValues <- 2^(datatypes$sizes*8 - as.integer(datatypes$isSigned)) - 1
        largestAbsoluteDataValue <- max(abs(max(data,na.rm=TRUE)), abs(min(data,na.rm=TRUE)))
        compatible <- compatible & (largestAbsoluteDataValue <= maximumValues)
        
        # Prefer Analyze-compatible data types for NIfTI files
        if (format == "Nifti" && any(compatible[datatypes$codes <= 64]))
            compatible <- compatible & (datatypes$codes <= 64)
        
        if (!any(compatible))
            report(OL$Error, "No compatible data type exists for the specified image and file format")
        
        maximumValues[!compatible] <- Inf
        code <- datatypes$codes[which.min(maximumValues)]
        size <- datatypes$sizes[datatypes$codes == code]
        isSigned <- datatypes$isSigned[datatypes$codes == code]
    }
    
    return (list(code=code, type=rType, size=size, isSigned=isSigned))
}

readImageFile <- function (fileName, fileType = NULL, metadataOnly = FALSE, volumes = NULL, sparse = FALSE, mask = NULL)
{
    fileNames <- identifyImageFileNames(fileName, fileType)
    
    readFun <- switch(fileNames$format, Analyze=readAnalyze, Nifti=readNifti, Mgh=readMgh)
    info <- readFun(fileNames)
    
    datatype <- info$storageMetadata$datatype
    endian <- info$storageMetadata$endian
    dims <- info$imageMetadata$imageDims
    voxelDims <- info$imageMetadata$voxelDims
    nVoxels <- prod(dims)
    nDims <- length(dims)
    
    if (sparse && !is.null(mask))
    {
        if (mask$getDimensionality() > nDims || mask$getDimensionality() < (nDims-1))
            report(OL$Error, "Mask must have the same number of dimensions as the image, or one fewer")
        else if (mask$getDimensionality() == nDims && !equivalent(mask$getDimensions(),dims))
            report(OL$Error, "Mask and image dimensions do not match")
        else if (mask$getDimensionality() == (nDims-1) && !equivalent(mask$getDimensions(),dims[-nDims]))
            report(OL$Error, "Mask and image dimensions do not match")
    }
    
    if (!is.null(volumes))
    {
        if (metadataOnly)
        {
            flag(OL$Warning, "Volumes specified when reading only metadata from an image file will be ignored")
            volumes <- NULL
        }
        else if (nDims != 4)
        {
            flag(OL$Warning, "Volumes specified for images with dimensionality other than 4 will be ignored")
            volumes <- NULL
        }
        else
        {
            if (any(volumes < 1 || volumes > prod(dims[4:nDims])))
                report(OL$Error, "Some of the specified volume numbers (", implode(volumes,","), ") are out of bounds")
            
            volumeSize <- prod(dims[1:3])
            jumps <- (diff(c(0, sort(volumes))) - 1) * volumeSize
            
            if (length(volumes) == 1)
            {
                dims <- dims[1:3]
                nDims <- 3
                voxelDims <- voxelDims[1:3]
            }
            else
            {
                matrixLocs <- vectorToMatrixLocs(volumes, dims[4:nDims])
                remainingVolumeDims <- apply(matrixLocs, 2, function (x) length(unique(x)))
                dims <- c(dims[1:3], remainingVolumeDims)
                
                dimsToKeep <- 1:max(which(dims > 1))
                dims <- dims[dimsToKeep]
                nDims <- length(dimsToKeep)
                voxelDims <- voxelDims[dimsToKeep]
            }
        }
    }
    
    if (!metadataOnly)
    {
        connection <- gzfile(fileNames$imageFile, "rb")
        if (fileNames$imageFile == fileNames$headerFile)
            readBin(connection, "raw", n=info$storageMetadata$dataOffset)
        
        if (sparse)
        {
            if (!is.null(volumes))
            {
                blocks <- volumes
                blockSize <- volumeSize
            }
            else
            {
                blocks <- 1:dims[nDims]
                jumps <- rep(0, length(blocks))
                blockSize <- prod(dims[-nDims])
            }
            
            coords <- NULL
            values <- NULL
            for (i in blocks)
            {
                if (jumps[i] > 0)
                    readBin(connection, "raw", n=jumps[i]*datatype$size)
                currentData <- readBin(connection, what=datatype$type, n=blockSize, size=datatype$size, signed=datatype$isSigned, endian=endian)
                
                toKeep <- which(currentData != 0)
                if (!is.null(mask) && mask$getDimensionality() == (nDims-1))
                    toKeep <- intersect(toKeep, which(mask$getData() > 0))
                if (length(toKeep) > 0)
                {
                    coords <- rbind(coords, cbind(vectorToMatrixLocs(toKeep,dims[-nDims]),i))
                    values <- c(values, currentData[toKeep])
                }
            }
            
            if (!is.null(mask) && mask$getDimensionality() == nDims)
            {
                toKeep <- which(matrixToVectorLocs(coords,dims) %in% which(mask$getData() > 0))
                coords <- coords[toKeep,]
                values <- values[toKeep]
            }
            
            data <- newSparseArrayWithData(values, coords, dims)
        }
        else if (!is.null(volumes))
        {
            data <- array(as(0,datatype$type), dim=c(dims[1:3],length(volumes)))
            for (i in seq_along(volumes))
            {
                if (jumps[i] > 0)
                    readBin(connection, "raw", n=jumps[i]*datatype$size)
                data[,,,i] <- readBin(connection, what=datatype$type, n=volumeSize, size=datatype$size, signed=datatype$isSigned, endian=endian)
            }
            dim(data) <- dims
        }
        else
        {
            voxels <- readBin(connection, what=datatype$type, n=nVoxels, size=datatype$size, signed=datatype$isSigned, endian=endian)
            data <- array(voxels, dim=dims)
        }
        
        close(connection)

        slope <- info$storageMetadata$dataScalingSlope
        intercept <- info$storageMetadata$dataScalingIntercept
        if (slope != 0 && !equivalent(c(slope,intercept), 1:0))
            data <- data * slope + intercept
    }
    
    rotationMatrix <- info$storageMetadata$xformMatrix[1:3,1:3]
    absRotationMatrix <- abs(rotationMatrix)
    tolerance <- 1e-3 * max(abs(voxelDims[1:min(3,nDims)]))
    
    # The rotation matrix should have exactly one nonzero element per row and column - if not, warn but try to figure out the closest primary orientation
    if (!equivalent(rowSums(absRotationMatrix > tolerance), c(1,1,1)) || !equivalent(colSums(absRotationMatrix > tolerance), c(1,1,1)))
    {
        flag(OL$Warning, "The image is stored in a rotated frame of reference")
        tolerance <- 0.5 * max(abs(voxelDims[1:min(3,nDims)]))
        if (!equivalent(rowSums(absRotationMatrix > tolerance), c(1,1,1)) || !equivalent(colSums(absRotationMatrix > tolerance), c(1,1,1)))
            report(OL$Error, "Cannot work out the primary orientation of the image")
    }
    
    dimPermutation <- apply(absRotationMatrix > tolerance, 1, which)
    if (nDims > 3)
        dimPermutation <- c(dimPermutation, 4:nDims)
    else if (nDims < 3)
        dimPermutation <- dimPermutation[1:nDims]
    if (!identical(dimPermutation, seq_len(nDims)))
    {
        if (!metadataOnly)
        {
            if (sparse)
                data$aperm(dimPermutation)
            else
                data <- aperm(data, dimPermutation)
        }
        dims <- dims[dimPermutation]
        voxelDims <- voxelDims[dimPermutation]
    }
        
    # Fix signs of voxel dimensions to correspond to LAS
    voxelDims <- abs(voxelDims) * c(-1, rep(1,nDims-1))
        
    # Figure out which dimensions need to be flipped - we sum by row because the data dimensions have already been permuted
    ordering <- round(rowSums(rotationMatrix) / c(abs(voxelDims[1:min(3,nDims)]),rep(1,max(0,3-nDims))))
    ordering <- ordering * c(-1, 1, 1)
        
    if (nDims == 2)
    {
        origin <- 1 - ordering[1:2] * round(info$storageMetadata$xformMatrix[1:2,4]/voxelDims[1:2],2)
        origin <- ifelse(ordering[1:2] == c(1,1), origin, dims-origin+1)
    }
    else
    {
        report(OL$Debug, "Image orientation is ", implode(c("I","P","R","","L","A","S")[(1:3)*ordering+4][dimPermutation[1:3]],sep=""))
        origin <- c(1 - ordering[1:3] * round(info$storageMetadata$xformMatrix[1:3,4]/voxelDims[1:3],2), rep(0,nDims-3))
        origin[1:3] <- ifelse(ordering[1:3] == c(1,1,1), origin[1:3], dims[1:3]-origin[1:3]+1)
    }
        
    if (!metadataOnly && any(ordering[1:min(3,nDims)] < 0))
    {
        if (sparse)
            data$flip(which(ordering[1:min(3,nDims)] < 0))
        else
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
    
    if (metadataOnly)
        image <- MriImage$new(imageDims=dims, voxelDims=voxelDims, voxelDimUnits=info$imageMetadata$voxelUnit, source=info$imageMetadata$source, origin=origin, storedXform=info$storageMetadata$xformMatrix, tags=info$imageMetadata$tags)
    else
        image <- MriImage$new(imageDims=dims, voxelDims=voxelDims, voxelDimUnits=info$imageMetadata$voxelUnit, source=info$imageMetadata$source, origin=origin, storedXform=info$storageMetadata$xformMatrix, tags=info$imageMetadata$tags, data=data)
    
    invisible (image)
}

newMriImageFromFile <- function (fileName, fileType = NULL, metadataOnly = FALSE, volumes = NULL, sparse = FALSE, mask = NULL)
{
    readImageFile(fileName, fileType, metadataOnly, volumes, sparse, mask)
}

writeImageData <- function (image, connection, type, size, endian = .Platform$endian)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    data <- image$getData()
    
    if (image$isSparse())
    {
        dims <- image$getDimensions()
        nDims <- image$getDimensionality()
        for (i in seq_len(dims[nDims]))
        {
            indices <- alist(x=,y=,z=,t=,u=,v=,w=)[1:nDims]
            indices[[nDims]] <- i
            currentData <- as.array(do.call("[", c(list(data),indices)))
            
            storage.mode(currentData) <- type
            attributes(currentData) <- NULL
            writeBin(currentData, connection, size=size, endian=endian)
        }
    }
    else
    {
        storage.mode(data) <- type
        attributes(data) <- NULL
        writeBin(data, connection, size=size, endian=endian)
    }
}

writeImageFile <- function (image, fileName = NULL, fileType = NA, overwrite = TRUE)
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
        writeMriImageToAnalyze(image, fileNames, gzipped=params$gzipped)
    else if (params$format == "Nifti")
        writeMriImageToNifti(image, fileNames, gzipped=params$gzipped)
    else if (params$format == "Mgh")
        writeMriImageToMgh(image, fileNames, gzipped=params$gzipped)
    
    invisible (fileNames)
}

writeMriImageToFile <- function (image, fileName = NULL, fileType = NA, overwrite = TRUE)
{
    writeImageFile(image, fileName, fileType, overwrite)
}
