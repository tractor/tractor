newMriImageFromCamino <- function (fileName, metadata)
{
    if (!is(metadata, "MriImageMetadata"))
        report(OL$Error, "The specified metadata is not an MriImageMetadata object")
    
    dims <- metadata$getDimensions()
    if (length(dims) == 4)
        caminoDims <- dims[c(4,1,2,3)]
    else
        caminoDims <- dims
    nVoxels <- prod(dims)
    
    suffix <- sub(".+\\.([BL][a-z]+)$", "\\1", fileName, perl=TRUE)
    endian <- ifelse(substr(suffix,1,1)=="B", "big", "little")
    typeIndex <- which(.Camino$typeNames == substr(suffix,2,nchar(suffix)))
    if (length(typeIndex) != 1)
        report(OL$Error, "The specified file name does not have a standard Camino suffix")
    datatype <- list(type=.Camino$rTypes[typeIndex], size=.Camino$sizes[typeIndex], isSigned=.Camino$isSigned[typeIndex])
    
    connection <- gzfile(fileName, "rb")
    voxels <- readBin(connection, what=datatype$type, n=nVoxels, size=datatype$size, signed=datatype$isSigned, endian=endian)
    close(connection)
    
    data <- array(voxels, dim=caminoDims)
    if (length(dims) == 4)
        data <- aperm(data, c(2,3,4,1))
    
    finalMetadata <- newMriImageMetadataFromTemplate(metadata, datatype=datatype, endian=endian)
    image <- newMriImageWithData(drop(data), finalMetadata)
    image$setSource(fileName)
    
    invisible (image)
}

writeMriImageToCamino <- function (image, fileName, gzipped = FALSE, datatype = NULL)
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
    datatypeMatches <- (.Camino$rTypes == datatype$type) & (.Camino$sizes == datatype$size) & (.Camino$isSigned == datatype$isSigned)
    if (sum(datatypeMatches) != 1)
    {
        signedMax <- 2^(datatype$size*8-1) - 1
        flipOkay <- (!datatype$isSigned && max(image) <= signedMax) || (datatype$isSigned && min(image) >= 0)
        if (flipOkay)
        {
            report(OL$Info, "Trying to change datatype to suit Camino")
            datatypeMatches <- (.Camino$rTypes == datatype$type) & (.Camino$sizes == datatype$size) & (.Camino$isSigned == !datatype$isSigned)
        }
    }
    if (sum(datatypeMatches) != 1)
        report(OL$Error, "No supported Camino datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- image$getData()
    storage.mode(data) <- .Camino$rTypes[typeIndex]
    
    # Camino uses voxel-ordered data files
    if (image$getDimensionality() == 4)
        data <- aperm(data, c(4,1,2,3))
    
    suffix <- paste("B", .Camino$typeNames[typeIndex], sep="")
    fileName <- ensureFileSuffix(fileName, suffix)
    
    # Image data
    connection <- fileFun(fileName, "w+b")
    writeBin(as.vector(data), connection, size=.Camino$sizes[typeIndex], endian="big")
    close(connection)
    
    invisible (fileName)
}
