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

print.metadata.dicom <- function (x, descriptions = FALSE, ...)
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
    
    if (is.na(rows))
        rows <- dataRows
    if (is.na(columns))
        columns <- dataColumns
    
    slices <- dicom$getTagValue(0x0019, 0x100a)
    if (is.na(slices))
    {
        if (rows == dataRows && columns == dataColumns)
            slices <- NULL
        else if (rows == dataColumns && columns == dataRows)
        {
            flag(OL$Info, "Data matrix is transposed relative to acquisition matrix")
            rows <- dataRows
            columns <- dataColumns
            slices <- NULL
        }
        else if (identical(dicom$getTagValue(0x0008,0x0070), "SIEMENS"))
        {
            # Siemens mosaic format
            slices <- (dataRows/rows) * (dataColumns/columns)
            if (slices != floor(slices))
            {
                flag(OL$Warning, "Image dimensions are not a multiple of the acquisition matrix size")
                slices <- NULL
                rows <- dataRows
                columns <- dataColumns
            }
        }
        else
        {
            # Image upsampled or downsampled after acquisition, e.g. by zero filling
            slices <- NULL
            rows <- dataRows
            columns <- dataColumns
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
        flag(OL$Warning, "Masking has altered the pixel values")
    
    return (newPixels)
}

newMriImageFromDicomMetadata <- function (metadata, flipY = TRUE)
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
        if (flipY)
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
        
        if (flipY)
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

readDiffusionParametersFromMetadata <- function (metadata)
{
    if (!isDicomMetadata(metadata))
        output(OL$Error, "The specified metadata is not a valid DicomMetadata object")
    
    bval <- metadata$getTagValue(0x0018, 0x9087)
    bvec <- metadata$getTagValue(0x0018, 0x9089)
    if (!is.na(bval))
    {
        if (bval == 0)
            return (list(bval=0, bvec=rep(0,3), defType="standard"))
        else if (!is.na(bvec))
            return (list(bval=bval, bvec=bvec, defType="standard"))
    }
    
    vendor <- metadata$getTagValue(0x0008, 0x0070)
    if (identical(vendor, "GE MEDICAL SYSTEMS"))
    {
        bval <- metadata$getTagValue(0x0043, 0x1039)[1]
        bvec <- c(metadata$getTagValue(0x0019, 0x10bb),
                  metadata$getTagValue(0x0019, 0x10bc),
                  metadata$getTagValue(0x0019, 0x10bd))
        
        if (is.na(bval))
            return (list(bval=NA, bvec=rep(NA,3), defType="none"))
        else if (bval == 0 || identical(bvec, rep(0,3)))
            return (list(bval=0, bvec=rep(0,3), defType="GE"))
        else
            return (list(bval=bval, bvec=bvec, defType="GE"))
    }
    else if (identical(vendor, "SIEMENS"))
    {
        bval <- metadata$getTagValue(0x0019, 0x100c)
        bvec <- metadata$getTagValue(0x0019, 0x100e)
        
        if (is.na(bval))
            return (list(bval=NA, bvec=rep(NA,3), defType="none"))
        else if (bval == 0 || identical(bvec, rep(0,3)))
            return (list(bval=0, bvec=rep(0,3), defType="Siemens"))
        else
            return (list(bval=bval, bvec=bvec, defType="Siemens"))
    }
    else
        return (list(bval=NA, bvec=rep(NA,3), defType="none"))
}

newMriImageFromDicomDirectory <- function (dicomDir, readDiffusionParams = FALSE)
{
    if (!file.exists(dicomDir) || !file.info(dicomDir)$isdir)
        output(OL$Error, "The specified path (", dicomDir, ") does not point to a directory")
    
    output(OL$Info, "Looking for DICOM files in directory ", dicomDir)
    files <- expandFileName(list.files(dicomDir, full.names=TRUE, recursive=TRUE))
    files <- files[!file.info(files)$isdir]
    nFiles <- length(files)
    
    data("dictionary", envir=environment(NULL))

    output(OL$Info, "Reading image information from ", nFiles, " files")
    seriesNumbers <- numeric(0)
    acquisitionNumbers <- numeric(0)
    imageNumbers <- numeric(0)
    sliceLocations <- numeric(0)
    bValues <- numeric(0)
    bVectors <- matrix(NA, nrow=3, ncol=0)
    images <- list()
    sliceDim <- sliceOrientation <- NULL
    count <- 0
    for (file in files)
    {
        metadata <- newDicomMetadataFromFile(file, dictionary=dictionary)
        if (is.null(metadata))
        {
            output(OL$Info, "Skipping ", file)
            next
        }
        else if (is.null(sliceDim))
        {
            sliceDim <- metadata$getTagValue(0x0018,0x0050)
            sliceOrientation <- metadata$getTagValue(0x0020,0x0037)
        }

        seriesNumbers <- c(seriesNumbers, metadata$getTagValue(0x0020,0x0011))
        acquisitionNumbers <- c(acquisitionNumbers, metadata$getTagValue(0x0020,0x0012))
        imageNumbers <- c(imageNumbers, metadata$getTagValue(0x0020,0x0013))
        sliceLocations <- c(sliceLocations, metadata$getTagValue(0x0020,0x1041))
        images <- c(images, list(newMriImageFromDicomMetadata(metadata, flipY=FALSE)))

        if (readDiffusionParams)
        {
            diffusion <- readDiffusionParametersFromMetadata(metadata)
            if (count == 0 && diffusion$defType != "none")
                output(OL$Info, "Attempting to read diffusion parameters using ", diffusion$defType, " DICOM convention")
            bValues <- c(bValues, diffusion$bval)
            bVectors <- cbind(bVectors, diffusion$bvec)
        }

        count <- count + 1
        if (count %% 100 == 0)
            output(OL$Verbose, "Done ", count)
    }

    nDicomFiles <- count
    if (nDicomFiles == 0)
        output(OL$Error, "No readable DICOM files were found")
    
    firstSliceDirection <- which(abs(sliceOrientation[1:3]) == 1) * sum(sliceOrientation[1:3])
    secondSliceDirection <- which(abs(sliceOrientation[4:6]) == 1) * sum(sliceOrientation[4:6])
    if (length(firstSliceDirection) != 1 || length(secondSliceDirection) != 1)
    {
        roundedSliceOrientation <- round(sliceOrientation)
        firstSliceDirection <- which(abs(roundedSliceOrientation[1:3]) == 1) * sum(roundedSliceOrientation[1:3])
        secondSliceDirection <- which(abs(roundedSliceOrientation[4:6]) == 1) * sum(roundedSliceOrientation[4:6])
        
        if (length(firstSliceDirection) == 1 || length(secondSliceDirection) == 1)
            output(OL$Warning, "Slices appear to be oblique: mean offset is ", signif(mean(abs(sliceOrientation-roundedSliceOrientation)),3))
        else
            output(OL$Error, "Slice orientation information is missing or complex")
    }
    
    sliceDirections <- abs(c(firstSliceDirection, secondSliceDirection))
    throughSliceDirection <- setdiff(1:3, abs(c(firstSliceDirection,secondSliceDirection)))
    output(OL$Info, "Slice select direction is ", LETTERS[24:26][throughSliceDirection])
    
    # If not TRUE, the data need flipping or transposing
    isSimpleCase <- (firstSliceDirection > 0 && secondSliceDirection > 0 && equivalent(setdiff(1:3,throughSliceDirection),sliceDirections))
    if (!isSimpleCase)
        output(OL$Info, "DICOM files do not use a \"simple\" slice arrangement")

    uniqueSeries <- sort(unique(seriesNumbers))
    uniqueAcquisitions <- sort(unique(acquisitionNumbers))
    uniqueImages <- sort(unique(imageNumbers))
    uniqueSlices <- sort(unique(sliceLocations))

    if (images[[1]]$getDimensionality() == 3)
    {
        volumePerDicomFile <- TRUE
        nSlices <- images[[1]]$getDimensions()[3]
        nVolumes <- nDicomFiles
        imageDims <- c(images[[1]]$getDimensions(), nVolumes)
        voxelDims <- c(images[[1]]$getVoxelDimensions(), 1)
    }
    else
    {
        volumePerDicomFile <- FALSE
        nSlices <- length(uniqueSlices)
        nVolumes <- nDicomFiles / nSlices
        if (floor(nVolumes) != nVolumes)
            output(OL$Error, "Number of files (", nDicomFiles, ") is not a multiple of the number of slices detected (", nSlices, ")")
        
        imageDims <- c(NA, NA, NA, nVolumes)
        imageDims[sliceDirections] <- images[[1]]$getDimensions()
        imageDims[throughSliceDirection] <- nSlices
        
        voxelDims <- c(NA, NA, NA, 1)
        voxelDims[sliceDirections] <- images[[1]]$getVoxelDimensions()
        voxelDims[throughSliceDirection] <- sliceDim
    }

    output(OL$Info, "Data set contains ", nVolumes, " volumes; ", nSlices, " slices per volume")
    data <- array(NA, dim=imageDims)

    if (readDiffusionParams)
    {
        volumeBValues <- rep(NA, nVolumes)
        volumeBVectors <- matrix(NA, nrow=3, ncol=nVolumes)
    }

    firstLocs <- numeric(0)

    # This is meant to be zero (so that the modulo arithmetic works)
    index <- 0
    for (s in uniqueSeries)
    {
        for (a in uniqueAcquisitions)
        {
            for (i in uniqueImages)
            {
                slicePos <- which(seriesNumbers==s & acquisitionNumbers==a & imageNumbers==i)
                if (length(slicePos) == 0)
                    next

                if (volumePerDicomFile)
                {
                    volume <- index + 1
                    data[,,,volume] <- images[[slicePos]]$getData()
                }
                else
                {
                    if (length(firstLocs) < 2)
                        firstLocs <- c(firstLocs, sliceLocations[slicePos])

                    slice <- (index %% nSlices) + 1
                    volume <- (index %/% nSlices) + 1
                    
                    # This generalised code is substantially slower than the
                    # special cases below, so we only use it if we have to
                    if (!isSimpleCase)
                    {
                        loc <- alist(x=, y=, z=, t=)
                        loc[[throughSliceDirection]] <- slice
                        if (firstSliceDirection < 0)
                            loc[[sliceDirections[1]]] <- imageDims[sliceDirections[1]]:1
                        if (secondSliceDirection < 0)
                            loc[[sliceDirections[2]]] <- imageDims[sliceDirections[2]]:1
                        loc$t <- volume

                        sliceData <- images[[slicePos]]$getData()
                        if (sliceDirections[1] > sliceDirections[2])
                            sliceData <- t(sliceData)

                        data <- do.call("[<-", c(list(data),loc,list(sliceData)))
                    }
                    else if (throughSliceDirection == 3)
                        data[,,slice,volume] <- images[[slicePos]]$getData()
                    else if (throughSliceDirection == 2)
                        data[,slice,,volume] <- images[[slicePos]]$getData()
                    else
                        data[slice,,,volume] <- images[[slicePos]]$getData()
                }

                if (readDiffusionParams && is.na(volumeBValues[volume]))
                {
                    volumeBValues[volume] <- bValues[slicePos]
                    volumeBVectors[,volume] <- bVectors[,slicePos]
                }

                index <- index + 1
            }
        }
    }

    if (!volumePerDicomFile && nSlices>1 && length(firstLocs)==2 && diff(firstLocs)<0)
    {
        output(OL$Info, "Slice location decreases between consecutive images - inverting slice order")
        indices <- alist(x=, y=, z=, t=)
        indices[[throughSliceDirection]] <- nSlices:1
        data <- do.call("[", c(list(data),indices,list(drop=FALSE)))
    }
    
    # DICOM uses LPS, we assume LAS (this call will also drop unitary
    # dimensions, typically the fourth)
    data <- data[,imageDims[2]:1,,,drop=TRUE]
    
    # Origin is at 1 for spatial dimensions (first 3), and 0 for temporal ones
    origin <- rep(1,length(imageDims))
    origin[setdiff(seq_along(origin),1:3)] <- 0

    dimsToKeep <- which(imageDims > 1)
    imageMetadata <- newMriImageMetadataFromTemplate(images[[1]]$getMetadata(), imageDims=imageDims[dimsToKeep], voxelDims=voxelDims[dimsToKeep], origin=origin[dimsToKeep])
    image <- newMriImageWithData(data, imageMetadata)
    
    returnValue <- list(image=image)
    if (readDiffusionParams)
        returnValue <- c(returnValue, list(bValues=volumeBValues, bVectors=volumeBVectors))
    
    invisible (returnValue)
}

newDicomMetadataFromFile <- function (fileName, checkFormat = TRUE, dictionary = NULL)
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
    
    if (is.null(dictionary))
        data("dictionary", envir=environment(NULL))
    dictionary$type <- as.vector(dictionary$type)
    typeCol <- which(colnames(dictionary) == "type")
    connection <- file(fileName, "rb")
    
    if (checkFormat)
    {
        seek(connection, where=128)
        str <- rawToCharQuiet(readBin(connection, "raw", n=4))
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
    type <- rawToCharQuiet(readBin(connection, "raw", n=2))
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
        
        sequenceLevel <- 0
        
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
            
            # Sequence related tags are always untyped
            if (currentGroup == 0xfffe)
            {
                # End of sequence delimiter
                if (sequenceLevel > 0 && currentElement == 0xe0dd)
                    sequenceLevel <- sequenceLevel - 1
                
                lengthSize <- 4
                type <- "UN"
            }
            else if (explicitTypes)
            {
                type <- rawToCharQuiet(readBin(connection, "raw", n=2))
                if (any(.Dicom$longTypes == type))
                {
                    seek(connection, where=2, origin="current")
                    lengthSize <- 4
                }
                else
                    lengthSize <- 2
            }
            else
            {
                lengthSize <- 4
                type <- dictionary$type[dictionary$group==currentGroup & dictionary$element==currentElement]
                if (length(type) == 0)
                    type <- "UN"
            }
            
            length <- readBin(connection, "integer", n=1, size=lengthSize, signed=FALSE, endian=endian)
            
            output(OL$Debug, "Group ", sprintf("0x%04x",currentGroup), ", element ", sprintf("0x%04x",currentElement), ", type ", type, ", length ", length, ifelse(sequenceLevel>0," (in sequence)",""))
            
            if (any(c("OX","OW","OB","UN") == type) || sequenceLevel > 0)
            {
                if ((currentGroup == 0x7fe0) && (currentElement == 0x0010))
                {
                    dataOffset <- seek(connection, where=NA)
                    dataLength <- length
                }
                
                if (type == "SQ" && length == -1)
                    sequenceLevel <- sequenceLevel + 1
                else if (length > 0)
                    seek(connection, where=length, origin="current")
                
                next
            }
            
            groups <- c(groups, currentGroup)
            elements <- c(elements, currentElement)
            types <- c(types, type)
            
            # Handle sequences of indeterminate length (to date only seen in Philips data)
            if (type == "SQ" && length == -1)
            {
                if (sequenceLevel == 0)
                    values <- c(values, "(Sequence)")
                sequenceLevel <- sequenceLevel + 1
                next
            }
            
            if (any(.Dicom$nonCharTypes$codes == type))
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
                values <- c(values, rawToCharQuiet(readBin(connection, "raw", n=length)))
        }
        
        close(connection)

        if (is.null(dataOffset))
            invisible (NULL)
        else
        {
            tags <- data.frame(groups=groups, elements=elements, types=types, values=values, stringsAsFactors=FALSE)
            invisible (.DicomMetadata(fileName, tags, tagOffset, dataOffset, dataLength, explicitTypes, endian))
        }
    }
    else
    {
        close(connection)
        invisible (NULL)
    }
}
