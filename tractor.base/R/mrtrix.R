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
    on.exit(close(connection))
    
    magic <- rawToChar(stripNul(readBin(connection, "raw", n=12)))
    assert(magic == "mrtrix image", "File #{fileNames$headerFile} does not appear to be a valid MRtrix image")
    
    fields <- rawToChar(stripNul(readBin(connection, "raw", n=endOffset-11)))
    match <- ore.search("^\\s*(\\w+): (.+)\\s*$", fields, all=TRUE)
    fields <- structure(as.list(match[,2]), names=match[,1])
    mergedFields <- list()
    for (fieldName in unique(names(fields)))
        mergedFields[[fieldName]] <- unlist(fields[names(fields) == fieldName], use.names=FALSE)
    
    pad <- function (x, minLength = 3L, value = 1)
    {
        length <- length(x)
        if (length >= minLength)
            return (x)
        else
            return (c(x, rep(value, minLength-length)))
    }
    
    # Extract and remove the specified field, splitting up elements
    getField <- function (name, split = "\\s*,\\s*", required = TRUE)
    {
        value <- mergedFields[[name]]
        if (required && is.null(value))
            report(OL$Error, "Required MRtrix header field \"#{name}\" is missing")
        else if (!is.null(value) && !is.null(split))
            value <- unlist(ore.split(split, value))
        mergedFields[[name]] <<- NULL
        return (value)
    }
    
    dims <- as.integer(getField("dim"))
    voxelDims <- as.numeric(getField("vox"))
    voxelDims[!is.finite(voxelDims)] <- 0
    
    layoutMatch <- ore.search("^(\\+|-)(\\d)$", getField("layout"), simplify=FALSE)
    signs <- ifelse(layoutMatch[,,1] == "+", 1, -1)
    axes <- as.integer(layoutMatch[,,2]) + 1L
    if (length(axes) > 3 && any(axes[4:length(axes)] != 4:length(axes)))
        report(OL$Error, "Images not stored in volume block order are not yet supported")
    
    layoutMatrix <- diag(c(0,0,0,1))
    layoutMatrix[cbind(1:3, pad(axes,3,3L)[1:3])] <- pad(signs,3)[1:3]
    
    xform <- diag(c(pad(abs(voxelDims),3)[1:3], 1)) %*% layoutMatrix
    if (!is.null(mergedFields$transform))
    {
        mrtrixTransform <- rbind(matrix(as.numeric(getField("transform")), nrow=3, ncol=4, byrow=TRUE), c(0,0,0,1))
        xform <- mrtrixTransform %*% xform
    }
    
    datatypeString <- as.character(getField("datatype"))
    if (datatypeString == "Bit" || datatypeString %~% "^C")
        report(OL$Error, "Bit and complex datatypes are not supported")
    datatypeMatch <- ore.search("^(U)?(Int|Float)(8|16|32|64)(LE|BE)?$", datatypeString)
    datatype <- list(code=0,
                     type=ifelse(datatypeMatch[,2]=="Int", "integer", "double"),
                     size=as.integer(datatypeMatch[,3]) / 8L,
                     isSigned=!is.na(datatypeMatch[,1]))
    endianString <- ifelse(is.na(datatypeMatch[,4]), "", datatypeMatch[,4])
    endian <- switch(endianString, LE="little", BE="big", .Platform$endian)
    
    fileField <- getField("file")
    fileMatch <- ore.search("^\\s*(\\S+) (\\d+)\\s*$", fileField)
    
    scaling <- as.numeric(getField("scaling", required=FALSE))
    if (length(scaling) == 0)
        scaling <- c(0, 1)
    
    # Create a default header
    header <- niftiHeader()
    
    nDims <- length(dims)
    header$dim[seq_len(nDims+1)] <- c(nDims, dims)
    qform(header) <- structure(xform, code=2L)
    header$pixdim[seq_len(nDims)+1] <- voxelDims
    
    storage <- list(offset=as.integer(fileMatch[,2]), intercept=scaling[1], slope=scaling[2], datatype=datatype, endian=endian)
    invisible (list(image=NULL, header=header, storage=storage))
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
