registerPathHandler("^(.+)@(\\w+)(/(\\w+))?", function(path) {
    # The match has to have been done just before calling this function (although this is not thread-safe)
    groups <- groups(ore.lastmatch())
    if (length(groups) == 2)
        newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[2])
    else if (length(groups) == 3)
        newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[3], groups[2])
    else
        return(NULL)
})

newMriImageFromCamino <- function (fileName, templateImage)
{
    if (!is(templateImage, "MriImage"))
        report(OL$Error, "The specified template image is not an MriImage object")
    
    dims <- templateImage$getDimensions()
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
    
    image <- asMriImage(drop(data), templateImage)
    image$setSource(fileName)
    
    invisible (image)
}

writeMriImageToCamino <- function (image, fileName, gzipped = FALSE, datatype = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    fileFun <- (if (gzipped) gzfile else file)
    
    # Every Analyze datatype has a valid Camino equivalent
    datatype <- tractor.base:::chooseDataTypeForImage(image, "Analyze")
    typeIndex <- which(.Camino$rTypes == datatype$type & .Camino$sizes == datatype$size & .Camino$isSigned == datatype$isSigned)
    
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
