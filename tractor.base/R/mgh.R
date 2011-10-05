readMgh <- function (fileNames)
{
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "File ", fileNames$headerFile, " not found")
    
    # The gzfile function can handle uncompressed files too
    connection <- gzfile(fileNames$headerFile, "rb")
    
    version <- readBin(connection, "integer", n=1, size=4, endian="big")
    if (version != 1)
        report(OL$Error, "Only version 1 MGH/MGZ files are supported")
    
    dims <- readBin(connection, "integer", n=4, size=4, endian="big")
    typeCode <- readBin(connection, "integer", n=1, size=4, endian="big")
    readBin(connection, "raw", n=4)
    orientationStored <- as.logical(readBin(connection, "integer", n=1, size=2, endian="big"))
    voxelDims <- readBin(connection, "double", n=3, size=4, endian="big")
    
    if (orientationStored)
    {
        xformMatrix <- matrix(readBin(connection, "double", n=12, size=4, endian="big"), nrow=3)
        xformMatrix <- rbind(xformMatrix, c(0,0,0,1))
    }
    else
        xformMatrix <- matrix(c(-1,0,0,0,0,0,-1,0,0,1,0,0,0,0,0,1), nrow=4)
    
    close(connection)
    
    diag(xformMatrix) <- diag(xformMatrix) * c(abs(voxelDims), 1)
    xformMatrix[,4] <- xformMatrix[,4] - c(dims[1]/2, dims[2]/2, dims[3]/2, 0) * diag(xformMatrix)
    
    typeIndex <- which(.Mgh$datatypes$codes == typeCode)
    if (length(typeIndex) != 1)
        report(OL$Error, "Specified MGH data type code is not valid")
    datatype <- list(type=.Mgh$datatypes$rTypes[typeIndex], size=.Mgh$datatypes$sizes[typeIndex], isSigned=.Mgh$datatypes$isSigned[typeIndex])
    
    dimsToKeep <- 1:max(which(dims > 1))
    imageMetadata <- list(imageDims=dims[dimsToKeep], voxelDims=voxelDims[dimsToKeep], voxelUnit=NULL, source=fileNames$fileStem, datatype=datatype, tags=list())
    
    storageMetadata <- list(dataOffset=284, dataScalingSlope=1, dataScalingIntercept=0, xformMatrix=xformMatrix, endian="big")
    
    invisible (list(imageMetadata=imageMetadata, storageMetadata=storageMetadata))
}

writeMriImageToMgh <- function (image, fileNames, gzipped = FALSE, datatype = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    fileFun <- (if (gzipped) gzfile else file)
    
    if (is.null(datatype))
    {
        datatype <- image$getDataType()
        if (is.null(datatype))
            report(OL$Error, "The data type is not stored with the image; it must be specified")
    }
    
    # Try to match the datatype exactly; failing that, and if the data will
    # fit, invert isSigned and try again; if that fails too, we have to give up
    datatypeMatches <- (.Mgh$datatypes$rTypes == datatype$type) & (.Mgh$datatypes$sizes == datatype$size) & (.Mgh$datatypes$isSigned == datatype$isSigned)
    if (sum(datatypeMatches) != 1)
    {
        signedMax <- 2^(datatype$size*8-1) - 1
        flipOkay <- (!datatype$isSigned && max(image) <= signedMax) || (datatype$isSigned && min(image) >= 0)
        if (flipOkay)
        {
            report(OL$Info, "Trying to change datatype for compatibility with MGH/MGZ format")
            datatypeMatches <- (.Mgh$datatypes$rTypes == datatype$type) & (.Mgh$datatypes$sizes == datatype$size) & (.Mgh$datatypes$isSigned == !datatype$isSigned)
        }
    }
    if (sum(datatypeMatches) != 1)
        report(OL$Error, "No supported MGH/MGZ datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- image$getData()
    if (.Mgh$datatypes$rTypes[typeIndex] == "integer")
        data <- as.integer(data)
    else
        data <- as.double(data)
    
    dims <- image$getDimensions()
    ndims <- image$getDimensionality()
    if (ndims > 4)
    {
        flag(OL$Warning, "The MGH/MGZ format can only handle 4 dimensions - the rest will be flattened")
        fullDims <- c(dims[1:3], prod(dims[4:ndims]))
    }
    else
        fullDims <- c(dims, rep(1,4-ndims))
    
    fullVoxelDims <- c(image$getVoxelDimensions(), rep(0,3-ndims))[1:3]
    
    origin <- image$getOrigin()
    if (length(origin) > 3)
        origin <- origin[1:3]
    else if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    origin <- (origin + fullDims[1:3]/2 - 1) * fullVoxelDims
    xformlikeMatrix <- matrix(c(-1, 0, 0, origin[1],
                                 0, 1, 0, origin[2],
                                 0, 0, 1, origin[3]), nrow=3, byrow=TRUE)
    
    connection <- fileFun(fileNames$headerFile, "w+b")
    
    writeBin(as.integer(1), connection, size=4, endian="big")
    writeBin(as.integer(fullDims), connection, size=4, endian="big")
    writeBin(as.integer(.Mgh$datatypes$codes[typeIndex]), connection, size=4, endian="big")
    writeBin(raw(4), connection)
    writeBin(as.integer(1), connection, size=2, endian="big")
    writeBin(as.double(abs(fullVoxelDims)), connection, size=4, endian="big")
    writeBin(as.double(xformlikeMatrix), connection, size=4, endian="big")
    writeBin(raw(194), connection)
    
    writeBin(data, connection, size=.Mgh$datatypes$sizes[typeIndex], endian="big")
    close(connection)
    
    if (image$isInternal())
        image$setSource(expandFileName(fileNames$fileStem))
}
