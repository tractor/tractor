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
    
    image <- newMriImageWithData(drop(data), templateImage)
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

readParcellation <- function (imageFileName, regionFileName = NULL, ...)
{
    image <- readImageFile(fileName, ...)
    if (is.null(regionFileName))
        regionFileName <- ensureFileSuffix(image$getSource(), "txt")
    regions <- read.table(regionFileName, header=TRUE, stringsAsFactors=FALSE)
    return (list(image=image, regions=regions))
}

writeParcellation <- function (image, regions, ...)
{
    if (!is.data.frame(regions))
        report(OL$Error, "Regions should be specified as a data frame")
    else if (nrow(regions) == 0)
        report(OL$Error, "Regions data frame is empty")
    
    info <- writeImageFile(image, ...)
    regionFileName <- ensureFileSuffix(info$fileStem, "txt")
    regionMatrix <- as.matrix(format(regions, justify="left"))
    regionMatrix[,"colour"] <- paste('"', regionMatrix[,"colour"], '"', sep="")
    
    nColumns <- ncol(regions)
    widths <- pmax(nchar(regionMatrix[1,]), nchar(colnames(regions)))
    
    padding <- widths - nchar(colnames(regions))
    headerString <- " "
    for (i in seq_len(nColumns))
        headerString <- paste(headerString, colnames(regions)[i], implode(rep(" ",padding[i]+2),sep=""), sep="")
    headerString <- substr(headerString, 1, nchar(headerString)-2)
    
    rightJustify <- sapply(seq_len(nColumns), function(i) is.numeric(regions[,i]))
    padding <- widths - nchar(regionMatrix[1,])
    lines <- apply(regionMatrix, 1, function(x) {
        string <- " "
        for (i in seq_along(x))
        {
            if (rightJustify[i])
                string <- paste(string, implode(rep(" ",padding[i]),sep=""), x[i], "  ", sep="")
            else
                string <- paste(string, x[i], implode(rep(" ",padding[i]+2),sep=""), sep="")
        }
        return (string)
    })
    lines <- c(headerString, implode(c("#",rep("-",sum(widths)+2*(length(widths)-1)))), lines)
    lines <- sub("\\s+$", "", lines, perl=TRUE)
    
    writeLines(lines, regionFileName)
}
