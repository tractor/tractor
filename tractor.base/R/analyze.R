newMriImageMetadataFromAnalyze <- function (fileNames)
{
    if (!file.exists(fileNames$headerFile))
        output(OL$Error, "Header file ", fileNames$headerFile, " not found")
    
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

    # SPM and FSL use the (char[10]) originator field to store a coordinate
    # origin - if not used as such this field should be all zero
    seek(connection, where=253)
    origin <- readBin(connection, "integer", n=5, size=2, endian=endian)

    close(connection)
    
    ndims <- dims[1]
    dims <- dims[1:ndims + 1]
    
    typeIndex <- which(.Analyze$typeCodes == typeCode)
    if (length(typeIndex) != 1)
        output(OL$Error, "Data type of file ", fileNames$imageFile, " (", typeCode, ") is not supported")
    datatype <- list(type=.Analyze$typesR[typeIndex], size=.Analyze$sizes[typeIndex], isSigned=.Analyze$isSigned[typeIndex])
    
    dimsToKeep <- which(dims > 1)
    metadata <- .MriImageMetadata(dims[dimsToKeep], voxelDims[dimsToKeep+1], NULL, fileNames$fileStem, datatype, origin[dimsToKeep], endian)
    
    invisible (metadata)
}

newMriImageFromAnalyze <- function (fileNames)
{
    if (!file.exists(fileNames$imageFile)) 
        output(OL$Error, "Image file ", fileNames$imageFile, " not found")
    
    metadata <- newMriImageMetadataFromAnalyze(fileNames)
    nVoxels <- prod(metadata$getDimensions())
    datatype <- metadata$getDataType()
    endian <- metadata$getEndianness()
    dims <- metadata$getDimensions()
    
    connection <- gzfile(fileNames$imageFile, "rb")
    voxels <- readBin(connection, what=datatype$type, n=nVoxels, size=datatype$size, signed=datatype$isSigned, endian=endian)
    data <- array(voxels, dim=dims)
    close(connection)
    
    image <- .MriImage(drop(data), metadata)
    invisible (image)
}

writeMriImageToAnalyze <- function (image, fileNames, gzipped = FALSE, datatype = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    fileFun <- (if (gzipped) gzfile else file)
    
    if (is.null(datatype))
    {
        datatype <- image$getDataType()
        if (is.null(datatype))
            output(OL$Error, "The data type is not stored with the image; it must be specified")
    }
    
    datatypeMatches <- (.Analyze$typesR == datatype$type) & (.Analyze$sizes == datatype$size) & (.Analyze$isSigned == datatype$isSigned)
    if (length(which(datatypeMatches == TRUE)) != 1)
        output(OL$Error, "No supported Analyze datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches == TRUE)
    
    data <- image$getData()
    if (.Analyze$typesR[typeIndex] == "integer")
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
    {
        image$setSource(expandFileName(fileNames$fileStem))
        image$setEndianness(.Platform$endian)
    }
}
