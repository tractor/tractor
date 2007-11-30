hasNiftiMagicString <- function (fileName)
{
    connection <- gzfile(fileName, "rb")
    seek(connection, where=344)
    magicString <- rawToChar(readBin(connection, "raw", n=4))
    close(connection)
    
    return (magicString %in% c("ni1\0","n+1\0"))
}

createNiftiMetadata <- function (fileNames)
{
    getRotationMatrixFromXform <- function (x, qfactor)
    {
        # The qform case
        if (is.vector(x))
        {
            if (sum(x) > 1 || sum(x %in% 0:1) != 3)
                output(OL$Error, "Only diagonal xform rotation matrices are supported")
            a <- sqrt(1 - sum(x^2))
            diagonal <- c(a^2+x[1]^2-x[2]^2-x[3]^2, a^2+x[2]^2-x[1]^2-x[3]^2, qfactor*(a^2+x[3]^2-x[1]^2-x[2]^2))
            return (diag(diagonal))
        }
        # The sform case
        else if (is.matrix(x))
        {
            if (!identical(abs(x), diag(3)))
                output(OL$Error, "Only diagonal xform rotation matrices are supported")
            x[3,3] <- qfactor * x[3,3]
            return (x)
        }
    }
    
    if (!file.exists(fileNames$headerFile))
        output(OL$Error, "Header file ", fileNames$headerFile, " not found")
        
    # The gzfile function can handle uncompressed files too
    connection <- gzfile(fileNames$headerFile, "rb")
    
    size <- readBin(connection, "integer")
    if (size == 348)
        endian <- .Platform$endian
    else
        endian <- setdiff(c("big","little"), .Platform$endian)

    seek(connection, where=40)
    dims <- readBin(connection, "integer", n=8, size=2, endian=endian)
    seek(connection, where=70)
    typeCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
    seek(connection, where=76)
    voxelDims <- readBin(connection, "double", n=8, size=4, endian=endian)
    dataOffset <- readBin(connection, "double", n=1, size=4, endian=endian)
    slopeAndIntercept <- readBin(connection, "double", n=2, size=4, endian=endian)
    seek(connection, where=123)
    unitCode <- readBin(connection, "integer", n=1, size=1, endian=endian)
    seek(connection, where=252)
    qformCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
    sformCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
    quaternionParams <- readBin(connection, "double", n=6, size=4, endian=endian)
    affine <- matrix(readBin(connection, "double", n=12, size=4, endian=endian), nrow=3, byrow=TRUE)
    seek(connection, where=344)
    magicString <- rawToChar(readBin(connection, "raw", n=4))

    close(connection)
    
    if (!(magicString %in% c("ni1\0","n+1\0")))
        output(OL$Error, "The file ", fileNames$headerFile, " is not a valid NIfTI file")
    
    ndims <- dims[1]
    dims <- dims[1:ndims + 1]
    
    if (qformCode == 0 && sformCode == 0)
    {
        output(OL$Debug, "Both qform and sform are zero - setting zero origin")
        origin <- rep(0,5)
        output(OL$Warning, "Nifti qform and sform codes are both zero in file ", fileNames$headerFile, " - orientation can only be guessed")
        # This guess is the same as the one that FSL uses - assume the file
        # uses the Analyze orientation convention
        rotationMatrix <- diag(c(-1,1,1))
    }
    else if (qformCode > 0)
    {
        output(OL$Debug, "Using qform (code ", qformCode, ") for origin")
        origin <- abs(round(quaternionParams[4:6] / voxelDims[2:4])) + 1
        origin <- c(origin, 0, 0)
        rotationMatrix <- getRotationMatrixFromXform(quaternionParams[1:3], voxelDims[1])
    }
    else
    {
        output(OL$Debug, "Using sform (code ", sformCode, ") for origin")
        origin <- abs(round(affine[,4] / voxelDims[2:4])) + 1
        origin <- c(origin, 0, 0)
        rotationMatrix <- getRotationMatrixFromXform(affine[,1:3]/diag(voxelDims[2:4]), voxelDims[1])
    }
    
    typeIndex <- which(.Nifti$datatypes$codes == typeCode)
    if (length(typeIndex) != 1)
        output(OL$Error, "Data type of file ", fileNames$imageFile, " (", typeCode, ") is not supported")
    datatype <- list(type=.Nifti$datatypes$rTypes[typeIndex], size=.Nifti$datatypes$sizes[typeIndex], isSigned=.Nifti$datatypes$isSigned[typeIndex])
    
    # We're only interested in the bottom 3 bits (the spatial unit)
    unitCode <- packBits(c(intToBits(unitCode)[1:3], intToBits(0L)[4:32]), "integer")
    unit <- names(.Nifti$units)[which(.Nifti$units == unitCode)]
    if (length(unit) == 0)
        unit <- NULL
    
    dimsToKeep <- which(dims > 1)
    imageMetadata <- .MriImageMetadata(dims[dimsToKeep], voxelDims[dimsToKeep+1], unit, fileNames$fileStem, datatype, origin[dimsToKeep], endian)
    
    storageMetadata <- list(dataOffset=dataOffset,
                            dataScalingSlope=slopeAndIntercept[1],
                            dataScalingIntercept=slopeAndIntercept[2],
                            rotationMatrix=rotationMatrix)
    
    invisible (list(imageMetadata=imageMetadata, storageMetadata=storageMetadata))
}

newMriImageMetadataFromNifti <- function (fileNames)
{
    nifti <- createNiftiMetadata(fileNames)
    invisible (nifti$imageMetadata)
}

newMriImageFromNifti <- function (fileNames)
{
    nifti <- createNiftiMetadata(fileNames)
    
    nVoxels <- prod(nifti$imageMetadata$getDimensions())
    datatype <- nifti$imageMetadata$getDataType()
    endian <- nifti$imageMetadata$getEndianness()
    dims <- nifti$imageMetadata$getDimensions()
    nDims <- nifti$imageMetadata$getDimensionality()
    
    connection <- gzfile(fileNames$imageFile, "rb")
    if (fileNames$imageFile == fileNames$headerFile)
        seek(connection, where=nifti$storageMetadata$dataOffset)
    
    voxels <- readBin(connection, what=datatype$type, n=nVoxels, size=datatype$size, signed=datatype$isSigned, endian=endian)
    data <- array(voxels, dim=dims)
    close(connection)
    
    slope <- nifti$storageMetadata$dataScalingSlope
    intercept <- nifti$storageMetadata$dataScalingIntercept
    if (slope != 0 && !equivalent(c(slope,intercept), 1:0))
        data <- data * slope + intercept
    
    # The first test is for -1 because basic NIfTI storage convention is RAS,
    # whilst Analyze (and TractoR) use LAS - this is NOT a mistake
    matrixDiag <- diag(nifti$storageMetadata$rotationMatrix)
    orderX <- (if (matrixDiag[1] == -1) seq_len(dims[1]) else rev(seq_len(dims[1])))
    orderY <- (if (matrixDiag[2] == 1) seq_len(dims[2]) else rev(seq_len(dims[2])))
    orderZ <- (if (matrixDiag[3] == 1) seq_len(dims[3]) else rev(seq_len(dims[3])))
    dimsToKeep <- setdiff(1:nDims, 1:3)
    
    if (nDims == 2)
        data <- data[orderX, orderY]
    else if (nDims == 3)
        data <- data[orderX, orderY, orderZ]
    else
        data <- array(apply(data, dimsToKeep, "[", orderX, orderY, orderZ), dim=dim(data))

    image <- .MriImage(drop(data), nifti$imageMetadata)
    invisible (image)
}

writeMriImageToNifti <- function (image, fileNames, gzipped = FALSE, datatype = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    description <- "Created by the TractoR NIfTI writer v1.0.0"
    fileFun <- (if (gzipped) gzfile else file)
    
    if (is.null(datatype))
    {
        datatype <- image$getDataType()
        if (is.null(datatype))
            output(OL$Error, "The data type is not stored with the image; it must be specified")
    }
    
    datatypeMatches <- (.Nifti$datatypes$rTypes == datatype$type) & (.Nifti$datatypes$sizes == datatype$size) & (.Nifti$datatypes$isSigned == datatype$isSigned)
    if (length(which(datatypeMatches)) != 1)
        output(OL$Error, "No supported NIfTI datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- image$getData()
    if (.Nifti$datatypes$rTypes[typeIndex] == "integer")
        data <- as.integer(data)
    else
        data <- as.double(data)
    
    ndims <- image$getDimensionality()
    fullDims <- c(ndims, image$getDimensions(), rep(1,7-ndims))
    fullVoxelDims <- c(-1, image$getVoxelDimensions(), rep(0,7-ndims))
    
    # The 8 below is for seconds; we default to 10 (mm/sec)
    unitName <- image$getVoxelUnit()
    unitCode <- as.numeric(.Nifti$units[which(names(.Nifti$units) == unitName)])
    if (length(unitCode) == 0)
        unitCode <- 10
    else
        unitCode <- unitCode + 8
    
    origin <- (image$getOrigin() - 1) * image$getVoxelDimensions()
    if (length(origin) > 3)
        origin <- origin[1:3]
    else if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    origin[2:3] <- -origin[2:3]
    sformRows <- c(-fullVoxelDims[2], 0, 0, origin[1],
                    0, fullVoxelDims[3], 0, origin[2],
                    0, 0, fullVoxelDims[4], origin[3])
    
    connection <- fileFun(fileNames$headerFile, "w+b")
    
    writeBin(as.integer(348), connection, size=4)
    writeBin(raw(36), connection)
    writeBin(as.integer(fullDims), connection, size=2)
    writeBin(raw(14), connection)
    writeBin(as.integer(.Nifti$datatypes$codes[typeIndex]), connection, size=2)
    writeBin(as.integer(8*.Nifti$datatypes$sizes[typeIndex]), connection, size=2)
    writeBin(raw(2), connection)
    writeBin(fullVoxelDims, connection, size=4)
    
    # Voxel offset, data scaling slope and intercept
    writeBin(as.double(c(352,1,0)), connection, size=4)
    
    writeBin(raw(3), connection)
    writeBin(as.integer(unitCode), connection, size=1)
    writeBin(raw(24), connection)
    writeBin(charToRaw(description), connection, size=1)
    writeBin(raw(24+80-nchar(description)), connection)
    
    # NIfTI xform block: sform and qform codes are hardcoded to 2 here
    writeBin(as.integer(c(2,2)), connection, size=2)
    writeBin(c(0,1,0,origin), connection, size=4)
    writeBin(sformRows, connection, size=4)
    
    writeBin(raw(16), connection)
    if (fileNames$imageFile == fileNames$headerFile)
    {
        writeBin(c(charToRaw("n+1"),as.raw(0)), connection, size=1)
        writeBin(raw(4), connection)
    }
    else
    {
        writeBin(c(charToRaw("ni1"),as.raw(0)), connection, size=1)
        close(connection)
        connection <- fileFun(fileNames$imageFile, "w+b")
    }
    
    writeBin(data, connection, size=.Nifti$datatypes$sizes[typeIndex])
    close(connection)
    
    if (image$isInternal())
    {
        image$setSource(expandFileName(fileNames$fileStem))
        image$setEndianness(.Platform$endian)
    }
}
