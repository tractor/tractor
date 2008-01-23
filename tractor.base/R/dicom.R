.DicomMetadata <- function (.source, .tags, .tagOffset, .dataOffset, .dataLength, .explicitTypes, .endian)
{
    self <- list(
        getAvailableTags = function ()
        {
            nTags <- self$nTags()
            tags <- list()
            if (nTags > 0)
            {
                for (i in 1:nTags)
                    tags <- c(tags, list(list(group=.tags[i,1],element=.tags[i,2])))
            }
            return (tags)
        },

        getDataLength = function () { return (.dataLength) },
        
        getDataOffset = function () { return (.dataOffset) },
        
        getEndianness = function () { return (.endian) },
        
        getSource = function () { return (.source) },
        
        getTagOffset = function () { return (.tagOffset) },
        
        getTagValue = function (group, element)
        {
            valueRow <- subset(.tags, (groups == group & elements == element))
            if (dim(valueRow)[1] == 0)
                return (NA)
            else
            {
                value <- unlist(strsplit(as.vector(valueRow$values), "\\", fixed=TRUE))
                if (capabilities()["iconv"] == TRUE)
                    value <- iconv(value, "", "LATIN1", sub="byte")
                value <- gsub("^\\s*(.+?)\\s*$", "\\1", value, perl=TRUE)
                
                if (as.vector(valueRow$types) %in% .Dicom$convertibleTypes)
                    return (as.numeric(value))
                else
                    return (value)
            }
        },

        nTags = function () { return (length(.tags[,1])) }
    )
    
    class(self) <- c("metadata.dicom", "list.object", "list")
    invisible (self)
}

isDicomMetadata <- function (object)
{
    return ("metadata.dicom" %in% class(object))
}

print.metadata.dicom <- function (x, descriptions = FALSE)
{
    tags <- x$getAvailableTags()
    if (descriptions && !exists("dictionary"))
        data("dictionary", envir=environment(NULL))
    
    if (length(tags) > 0)
    {
        if (descriptions)
        {
            cat("DESCRIPTION", rep(" ",19), "VALUE\n", sep="")
            for (tag in tags)
            {
                description <- getDescriptionForDicomTag(tag$group, tag$element, dictionary)
                cat(" ", substr(description, 1, 27), sep="")
                nSpaces <- max(3, 30-nchar(description))
                cat(rep(" ",nSpaces), sep="")
                cat(implode(x$getTagValue(tag$group,tag$element), sep=", "))
                cat("\n")
            }
        }
        else
        {
            cat("GROUP    ELEMENT  VALUE\n")
            for (tag in tags)
            {
                cat(sprintf(" 0x%04x   0x%04x   ", tag$group, tag$element))
                cat(implode(x$getTagValue(tag$group,tag$element), sep=", "))
                cat("\n")
            }
        }
    }
}

getDescriptionForDicomTag <- function (groupRequired, elementRequired, dictionary = NULL)
{
    if (is.null(dictionary))
        data("dictionary", envir=environment(NULL))
    
    dictionaryRow <- subset(dictionary, (group==groupRequired & element==elementRequired))
    if (nrow(dictionaryRow) == 0)
        description <- sprintf("Unknown (0x%04x, 0x%04x)", groupRequired, elementRequired)
    else
        description <- as.character(dictionaryRow$description)
    
    return (description)
}

newMriImageMetadataFromDicom <- function (fileName)
{
    fileMetadata <- newDicomMetadataFromFile(fileName)
    if (fileMetadata$getTagValue(0x0008, 0x0060) != "MR")
        output(OL$Error, "DICOM file does not contain MR image data")
    
    invisible (newMriImageMetadataFromDicomMetadata(fileMetadata))
}

newMriImageMetadataFromDicomMetadata <- function (dicom)
{
    acquisitionMatrix <- dicom$getTagValue(0x0018, 0x1310)
    rows <- max(acquisitionMatrix[1], acquisitionMatrix[3])
    columns <- max(acquisitionMatrix[2], acquisitionMatrix[4])
    dataRows <- dicom$getTagValue(0x0028, 0x0010)
    dataColumns <- dicom$getTagValue(0x0028, 0x0011)
    voxdims <- dicom$getTagValue(0x0028, 0x0030)
    endian <- dicom$getEndianness()
    
    slices <- dicom$getTagValue(0x0019, 0x100a)
    if (is.na(slices))
    {
        if (rows == dataRows && columns == dataColumns)
            slices <- NULL
        else
        {
            slices <- (dataRows/rows) * (dataColumns/columns)
            if (slices != floor(slices))
                output(OL$Error, "Image dimensions are not a multiple of the acquisition matrix size")
        }
    }
    
    if (is.null(slices))
        nDims <- 2
    else
    {
        nDims <- 3
        voxdims <- c(voxdims, dicom$getTagValue(0x0018, 0x0050))
    }
    
    bitsAllocated <- dicom$getTagValue(0x0028, 0x0100)
    if ((bitsAllocated %% 8) != 0)
        output(OL$Error, "Number of bits allocated per pixel doesn't correspond to an integral number of bytes")
    isSigned <- isTRUE(dicom$getTagValue(0x0028, 0x0103) == 1)
    datatype <- list(type="integer", size=bitsAllocated/8, isSigned=isSigned)
    
    metadata <- .MriImageMetadata(c(columns,rows,slices), voxdims, "mm", dicom$getSource(), datatype, rep(1,nDims), endian) 
    invisible (metadata)
}

maskPixels <- function (pixels, metadata)
{
    if (!is.numeric(pixels) || !is.vector(pixels))
        output(OL$Error, "Pixels must be specified as a numeric vector")
    if (!isDicomMetadata(metadata))
        output(OL$Error, "Specified metadata is not a valid DicomMetadata object")
    
    bitsAllocated <- metadata$getTagValue(0x0028, 0x0100)
    bitsStored <- metadata$getTagValue(0x0028, 0x0101)
    highBit <- metadata$getTagValue(0x0028, 0x0102)
    
    if (bitsAllocated == bitsStored)
        return (pixels)
    else if (!is.integer(pixels))
        output(OL$Error, "Pixels must be specified as an integer vector")
    
    mask <- rep(0, bitsAllocated)
    validIndices <- (1:bitsStored) + highBit - bitsStored + 1
    mask[validIndices] <- 1
    
    newPixels <- packBits(as.raw(mask) & intToBits(pixels), "integer")
    if (!equivalent(pixels, newPixels))
        output(OL$Warning, "Masking has altered the pixel values")
    
    return (newPixels)
}

newMriImageFromDicomMetadata <- function (metadata)
{
    fileMetadata <- metadata
    if (fileMetadata$getTagValue(0x0008, 0x0060) != "MR")
        output(OL$Error, "DICOM file does not contain MR image data")
    imageMetadata <- newMriImageMetadataFromDicomMetadata(fileMetadata)
    
    datatype <- imageMetadata$getDataType()
    nPixels <- fileMetadata$getDataLength() / datatype$size
    dims <- imageMetadata$getDimensions()
    nDims <- imageMetadata$getDimensionality()
    endian <- fileMetadata$getEndianness()
    
    connection <- file(fileMetadata$getSource(), "rb")
    seek(connection, where=fileMetadata$getDataOffset())
    pixels <- readBin(connection, "integer", n=nPixels, size=datatype$size, signed=datatype$isSigned, endian=endian)
    pixels <- maskPixels(pixels, fileMetadata)
    close(connection)
    
    if (nDims == 2)
    {
        data <- array(pixels, dim=dims)
        data <- data[,(dims[2]:1)]
    }
    else if (nDims == 3)
    {
        # Handle Siemens mosaic images, which encapsulate a whole 3D image in
        # a single-frame DICOM file
        mosaicDims <- c(fileMetadata$getTagValue(0x0028, 0x0010), fileMetadata$getTagValue(0x0028, 0x0011))
        mosaicGrid <- mosaicDims / dims[1:2]
        mosaicCellDims <- mosaicDims / mosaicGrid
        gridColumns <- rep(1:mosaicGrid[2], times=mosaicDims[2], each=mosaicCellDims[1])
        gridRows <- rep(1:mosaicGrid[1], each=mosaicCellDims[2]*mosaicDims[1])
        
        data <- array(NA, dim=dims)
        sliceList <- tapply(pixels, list(gridColumns,gridRows), "[")
        for (i in seq_len(dims[3]))
            data[,,i] <- sliceList[[i]]
        
        data <- data[,(dims[2]:1),]
    }
    
    image <- .MriImage(drop(data), imageMetadata)
    invisible (image)
}

newMriImageFromDicom <- function (fileName)
{
    fileMetadata <- newDicomMetadataFromFile(fileName)
    invisible (newMriImageFromDicomMetadata(fileMetadata))
}

newDicomMetadataFromFile <- function (fileName, checkFormat = TRUE)
{
    fileName <- expandFileName(fileName)
    
    if (!file.exists(fileName))
        output(OL$Error, "DICOM file ", fileName, " not found")
    
    # DICOM is sufficiently complicated that this can really only be
    # interpreted to mean "probably" or "probably not"
    isDicomFile <- !checkFormat
    endian <- .Platform$endian
    tagOffset <- 0
    dataOffset <- NULL
    
    data("dictionary", envir=environment(NULL))
    connection <- file(fileName, "rb")
    
    if (checkFormat)
    {
        seek(connection, where=128)
        str <- rawToChar(readBin(connection, "raw", n=4))
        if (str == "DICM")
        {
            isDicomFile <- TRUE
            tagOffset <- 132
        }
        else
            seek(connection, where=0)
    }
    
    group <- readBin(connection, "integer", n=1, size=2, signed=FALSE)
    if (group == 0x0008)
        isDicomFile <- TRUE
    else if (group == 0x0800)
        isDicomFile <- TRUE
        
    if (group > 0x00ff)
        endian <- setdiff(c("big","little"), .Platform$endian)
    
    seek(connection, where=2, origin="current")
    type <- rawToChar(readBin(connection, "raw", n=2))
    if (type == "UL")
        explicitTypes <- TRUE
    else
        explicitTypes <- FALSE
    
    if (isDicomFile)
    {
        groups <- numeric(0)
        elements <- numeric(0)
        types <- character(0)
        values <- character(0)
        
        seek(connection, where=tagOffset)
        repeat
        {
            currentGroup <- readBin(connection, "integer", n=1, size=2, signed=FALSE, endian=endian)
            if (length(currentGroup) == 0)
                break
            else if (currentGroup == 0x0800)
            {
                currentGroup <- 0x0008
                endian <- setdiff(c("big","little"), endian)
            }
            currentElement <- readBin(connection, "integer", n=1, size=2, signed=FALSE, endian=endian)
            
            if (explicitTypes)
            {
                type <- rawToChar(readBin(connection, "raw", n=2))
                if (type %in% .Dicom$longTypes)
                {
                    seek(connection, where=2, origin="current")
                    lengthSize <- 4
                }
                else
                    lengthSize <- 2
            }
            else
            {
                dictionaryRow <- subset(dictionary, (group==currentGroup & element==currentElement))
                lengthSize <- 4
                if (nrow(dictionaryRow) == 0)
                    type <- "UN"
                else
                    type <- as.vector(dictionaryRow$type)
            }
            
            length <- readBin(connection, "integer", n=1, size=lengthSize, signed=FALSE, endian=endian)
            
            if (type %in% c("OX","OW","OB","UN"))
            {
                if ((currentGroup == 0x7fe0) && (currentElement == 0x0010))
                {
                    dataOffset <- seek(connection, where=NA)
                    dataLength <- length
                }
                seek(connection, where=length, origin="current")
                next
            }
            
            groups <- c(groups, currentGroup)
            elements <- c(elements, currentElement)
            types <- c(types, type)
            
            if (type %in% .Dicom$nonCharTypes$codes)
            {
                loc <- which(.Dicom$nonCharTypes$codes == type)
                size <- .Dicom$nonCharTypes$sizes[loc]
                nValues <- length/size
                
                if (.Dicom$nonCharTypes$rTypes[loc] == "integer")
                    value <- readBin(connection, "integer", n=nValues, size=size, signed=.Dicom$nonCharTypes$isSigned[loc], endian=endian)
                else
                    value <- readBin(connection, "double", n=nValues, size=size, endian=endian)
                    
                if (nValues > 1)
                    values <- c(values, implode(value,sep="\\"))
                else
                    values <- c(values, as.character(value))
            }
            else
                values <- c(values, rawToChar(readBin(connection, "raw", n=length)))
        }
        
        close(connection)

        if (is.null(dataOffset))
            invisible (NULL)
        else
        {
            tags <- data.frame(groups, elements, types, values)
            invisible (.DicomMetadata(fileName, tags, tagOffset, dataOffset, dataLength, explicitTypes, endian))
        }
    }
    else
    {
        close(connection)
        invisible (NULL)
    }
}
