readAnalyze <- function (fileNames)
{
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "Header file ", fileNames$headerFile, " not found")
    
    connection <- gzfile(fileNames$headerFile, "rb")
    size <- readBin(connection, "integer", size=4)
    if (size == 348)
        endian <- .Platform$endian
    else
        endian <- setdiff(c("big","little"), .Platform$endian)

    readBin(connection, "raw", n=36)
    dims <- readBin(connection, "integer", n=8, size=2, endian=endian)
    readBin(connection, "raw", n=14)
    typeCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
    readBin(connection, "raw", n=4)
    voxelDims <- readBin(connection, "double", n=8, size=4, endian=endian)

    # SPM and FSL use the (char[10]) originator field to store a coordinate
    # origin - if not used as such this field should be all zero
    readBin(connection, "raw", n=145)
    origin <- readBin(connection, "integer", n=5, size=2, endian=endian)

    close(connection)
    
    ndims <- dims[1]
    dims <- dims[1:ndims + 1]
    
    typeIndex <- which(.Analyze$typeCodes == typeCode)
    if (length(typeIndex) != 1)
        report(OL$Error, "Data type of file ", fileNames$imageFile, " (", typeCode, ") is not supported")
    datatype <- list(type=.Analyze$rTypes[typeIndex], size=.Analyze$sizes[typeIndex], isSigned=.Analyze$isSigned[typeIndex])
    
    dimsToKeep <- 1:max(which(dims > 1))
    imageMetadata <- list(imageDims=dims[dimsToKeep], voxelDims=voxelDims[dimsToKeep+1], voxelUnit=NULL, source=fileNames$fileStem, datatype=datatype, tags=list())
    
    xformMatrix <- diag(c(-1,1,1,1) * abs(c(voxelDims[2:4],1)))
    xformMatrix[1:3,4] <- pmax(0,origin[1:3]-1) * abs(voxelDims[2:4]) * c(1,-1,-1)
    storageMetadata <- list(dataOffset=0, dataScalingSlope=1, dataScalingIntercept=0, xformMatrix=xformMatrix, endian=endian)
    
    invisible (list(imageMetadata=imageMetadata, storageMetadata=storageMetadata))
}

writeMriImageToAnalyze <- function (image, fileNames, gzipped = FALSE, datatype = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    fileFun <- (if (gzipped) gzfile else file)
    
    if (is.null(datatype))
    {
        datatype <- image$getDataType()
        if (length(datatype) == 0)
            report(OL$Error, "The data type is not stored with the image; it must be specified")
    }
    
    # Try to match the datatype exactly; failing that, and if the data will
    # fit, invert isSigned and try again; if that fails too, we have to give up
    datatypeMatches <- (.Analyze$rTypes == datatype$type) & (.Analyze$sizes == datatype$size) & (.Analyze$isSigned == datatype$isSigned)
    if (sum(datatypeMatches) != 1)
    {
        signedMax <- 2^(datatype$size*8-1) - 1
        flipOkay <- (!datatype$isSigned && max(image) <= signedMax) || (datatype$isSigned && min(image) >= 0)
        if (flipOkay)
        {
            report(OL$Info, "Trying to change datatype to comply with Analyze standard")
            datatypeMatches <- (.Analyze$rTypes == datatype$type) & (.Analyze$sizes == datatype$size) & (.Analyze$isSigned == !datatype$isSigned)
        }
    }
    if (sum(datatypeMatches) != 1)
        report(OL$Error, "No supported Analyze datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- image$getData()
    if (.Analyze$rTypes[typeIndex] == "integer")
        data <- as.integer(data)
    else
        data <- as.double(data)
    maxValue <- max(data)
    minValue <- min(data)
    
    ndims <- image$getDimensionality()
    fullDims <- c(ndims, image$getDimensions(), rep(1,7-ndims))
    fullVoxelDims <- c(0, image$getVoxelDimensions(), rep(0,7-ndims))
    
    origin <- image$getOrigin()
    fullOrigin <- c(origin, rep(0,5-length(origin)))
    
    connection <- fileFun(fileNames$headerFile, "w+b")
    
    # First substructure: header size, extents, "regular" indicator
    writeBin(as.integer(348), connection, size=4)
    writeBin(raw(28), connection)
    writeBin(as.integer(16384), connection, size=4)
    writeBin(raw(2), connection)
    writeBin(charToRaw("r"), connection, size=1)
    writeBin(raw(1), connection)
    
    # Second substructure: data dimensions, type
    writeBin(as.integer(fullDims), connection, size=2)
    writeBin(raw(14), connection)
    writeBin(as.integer(.Analyze$typeCodes[typeIndex]), connection, size=2)
    writeBin(as.integer(8*.Analyze$sizes[typeIndex]), connection, size=2)
    writeBin(raw(2), connection)
    writeBin(fullVoxelDims, connection, size=4)
    writeBin(raw(40), connection)
    
    # Third substructure: data history (includes origin)
    writeBin(raw(105), connection)
    writeBin(as.integer(fullOrigin), connection, size=2)
    writeBin(raw(85), connection)
    
    close(connection)
    
    # Image data
    connection <- fileFun(fileNames$imageFile, "w+b")
    writeBin(data, connection, size=.Analyze$sizes[typeIndex])
    close(connection)
    
    if (image$isInternal())
        image$setSource(expandFileName(fileNames$fileStem))
}
