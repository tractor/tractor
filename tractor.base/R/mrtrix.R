readMrtrix <- function (fileNames)
{
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "File #{fileNames$headerFile} not found")
    
    # Find the end of the header
    match <- ore.search("\nEND\n", ore.file(fileNames$headerFile, binary=TRUE))
    assert(!is.null(match), "File #{fileNames$headerFile} does not seem to contain a well-formed MRtrix header")
    endOffset <- match$byteOffsets
    
    # The gzfile function can handle uncompressed files too
    connection <- gzfile(fileNames$headerFile, "rb")
    
    magic <- rawToChar(stripNul(readBin(connection, "raw", n=12)))
    assert(magic == "mrtrix image", "File #{fileNames$headerFile} does not appear to be a valid MRtrix image")
    
    fields <- rawToChar(stripNul(readBin(connection, "raw", n=endOffset-11)))
    match <- ore.search("^\\s*(\\w+): (.+)\\s*$", fields, all=TRUE)
    fields <- structure(as.list(match[,2]), names=match[,1])
    mergedFields <- list()
    for (fieldName in unique(names(fields)))
        mergedFields[[fieldName]] <- unlist(fields[names(fields) == fieldName], use.names=FALSE)
    
    # Create a default header
    # header <- niftiHeader()
    #
    # dims <- readBin(connection, "integer", n=4, size=4, endian="big")
    # typeCode <- readBin(connection, "integer", n=1, size=4, endian="big")
    # readBin(connection, "raw", n=4)
    # orientationStored <- as.logical(readBin(connection, "integer", n=1, size=2, endian="big"))
    # voxelDims <- readBin(connection, "double", n=3, size=4, endian="big")
    #
    # scaleMatrix <- diag(c(abs(voxelDims[1:3]), 1))
    # if (orientationStored)
    # {
    #     # The stored xform is not scaled by the voxel dimensions
    #     xform <- matrix(readBin(connection, "double", n=12, size=4, endian="big"), nrow=3)
    #     xform <- rbind(xform, c(0,0,0,1)) %*% scaleMatrix
    # }
    # else
    # {
    #     # Default orientation is LIA
    #     xform <- scaleMatrix
    #     orientation(xform) <- "LIA"
    # }
    #
    # # Whether the information is stored or not, an origin adjustment is needed,
    # # because MGH format uses the image centre as a reference point
    # xform[1:3,4] <- xform[1:3,4] - (xform[1:3,1:3] %*% (dims[1:3] / 2))
    #
    # close(connection)
    #
    # nDims <- max(which(dims > 1))
    # if (nDims < 2)
    #     report(OL$Error, "MGH image appears to have less than two non-unitary dimensions")
    # header$dim[seq_len(nDims+1)] <- c(nDims, dims[1:nDims])
    # qform(header) <- structure(xform, code=2L)
    # header$pixdim[seq_len(nDims)+1] <- voxelDims[1:nDims]
    #
    # typeIndex <- which(.Mgh$datatypes$codes == typeCode)
    # if (length(typeIndex) != 1)
    #     report(OL$Error, "The MGH data type code is not valid")
    # datatype <- list(code=typeCode, type=.Mgh$datatypes$rTypes[typeIndex], size=.Mgh$datatypes$sizes[typeIndex], isSigned=.Mgh$datatypes$isSigned[typeIndex])
    #
    # storage <- list(offset=284, slope=1, intercept=0, datatype=datatype, endian="big")
    # invisible (list(image=NULL, header=header, storage=storage))
}

writeMrtrix <- function (image, fileNames, gzipped = FALSE)
{
    image <- as(image, "MriImage")
    
    fileFun <- (if (gzipped) gzfile else file)
    
    # datatype <- chooseDataTypeForImage(image, "Mgh")
    #
    # dims <- image$getDimensions()
    # ndims <- image$getDimensionality()
    # if (ndims > 4)
    # {
    #     flag(OL$Warning, "The MGH/MGZ format can only handle 4 dimensions - the rest will be flattened")
    #     fullDims <- c(dims[1:3], prod(dims[4:ndims]))
    # }
    # else
    #     fullDims <- c(dims, rep(1,4-ndims))
    #
    # fullVoxelDims <- c(image$getVoxelDimensions(), rep(0,3-ndims))[1:3]
    #
    # assert(image$isReordered(), "Unreordered images cannot currently be written to MGH/MGZ format")
    #
    # # We can assume the image is reordered here
    # origin <- image$getXform()[1:3,4] + fullDims[1:3]/2 * fullVoxelDims * c(-1,1,1)
    # xformlikeMatrix <- matrix(c(-1, 0, 0, origin[1],
    #                              0, 1, 0, origin[2],
    #                              0, 0, 1, origin[3]), nrow=3, byrow=TRUE)
    #
    # connection <- fileFun(fileNames$headerFile, "w+b")
    #
    # writeBin(as.integer(1), connection, size=4, endian="big")
    # writeBin(as.integer(fullDims), connection, size=4, endian="big")
    # writeBin(as.integer(datatype$code), connection, size=4, endian="big")
    # writeBin(raw(4), connection)
    # writeBin(as.integer(1), connection, size=2, endian="big")
    # writeBin(as.double(abs(fullVoxelDims)), connection, size=4, endian="big")
    # writeBin(as.double(xformlikeMatrix), connection, size=4, endian="big")
    # writeBin(raw(194), connection)
    #
    # writeImageData(image, connection, datatype$type, datatype$size, endian="big")
    # close(connection)
    #
    # if (image$isInternal())
    #     image$setSource(expandFileName(fileNames$fileStem))
}
