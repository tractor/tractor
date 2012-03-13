DicomMetadata <- setRefClass("DicomMetadata", contains="SerialisableObject", fields=list(source="character",tags="data.frame",tagOffset="integer",dataOffset="integer",dataLength="integer",explicitTypes="logical",endian="character"), methods=list(
    getDataLength = function () { return (dataLength) },
    
    getDataOffset = function () { return (dataOffset) },
    
    getEndianness = function () { return (endian) },
    
    getSource = function () { return (source) },
    
    getTags = function () { return (tags) },
    
    getTagOffset = function () { return (tagOffset) },
    
    getTagValue = function (group, element)
    {
        valueRow <- subset(tags, (tags$groups == group & tags$elements == element))
        if (dim(valueRow)[1] == 0 || valueRow$values == "")
            return (NA)
        else
        {
            value <- unlist(strsplit(as.vector(valueRow$values), "\\", fixed=TRUE, useBytes=TRUE))
            if (capabilities("iconv") == TRUE)
                value <- iconv(value, "", "LATIN1", sub="byte")
            value <- gsub("^\\s*(.+?)\\s*$", "\\1", value, perl=TRUE)
            
            if (as.vector(valueRow$types) %in% .Dicom$convertibleTypes)
                return (as.numeric(value))
            else
                return (value)
        }
    },

    nTags = function () { return (nrow(tags)) }
))

print.DicomMetadata <- function (x, descriptions = FALSE, ...)
{
    tags <- x$getTags()
    nTags <- nrow(tags)
    if (descriptions && !exists("dictionary"))
    {
        # First set to NULL to keep package checker happy
        dictionary <- NULL
        data("dictionary", package="tractor.base", envir=environment(NULL))
    }
    
    if (nTags > 0)
    {
        if (descriptions)
        {
            cat("DESCRIPTION", rep(" ",19), "VALUE\n", sep="")
            for (i in seq_len(nTags))
            {
                description <- getDescriptionForDicomTag(tags$groups[i], tags$elements[i], dictionary)
                cat(" ", substr(description, 1, 27), sep="")
                nSpaces <- max(3, 30-nchar(description))
                cat(rep(" ",nSpaces), sep="")
                cat(implode(x$getTagValue(tags$groups[i],tags$elements[i]), sep=", "))
                cat("\n")
            }
        }
        else
        {
            cat("GROUP    ELEMENT  VALUE\n")
            for (i in seq_len(nTags))
            {
                cat(sprintf(" 0x%04x   0x%04x   ", tags$groups[i], tags$elements[i]))
                cat(implode(x$getTagValue(tags$groups[i],tags$elements[i]), sep=", "))
                cat("\n")
            }
        }
    }
}

getDescriptionForDicomTag <- function (groupRequired, elementRequired, dictionary = NULL)
{
    if (is.null(dictionary))
        data("dictionary", envir=environment(NULL))
    
    dictionaryRow <- subset(dictionary, (dictionary$group==groupRequired & dictionary$element==elementRequired))
    if (nrow(dictionaryRow) == 0)
        description <- sprintf("Unknown (0x%04x, 0x%04x)", groupRequired, elementRequired)
    else
        description <- as.character(dictionaryRow$description)
    
    return (description)
}

sortDicomDirectory <- function (directory, deleteOriginals = FALSE, sortOn = "series")
{
    if (!file.exists(directory) || !file.info(directory)$isdir)
        report(OL$Error, "Specified path (", directory, ") does not exist or does not point to a directory")
    
    sortOn <- match.arg(sortOn, c("series","subject"), several.ok=TRUE)
    currentSort <- sortOn[1]
    remainingSorts <- sortOn[-1]
    identifierTag <- switch(currentSort, series=c(0x0020,0x0011), subject=c(0x0010,0x0010))
    descriptionTag <- switch(currentSort, series=c(0x0008,0x103e), subject=c(0x0010,0x0010))
    
    directory <- expandFileName(directory)
    files <- expandFileName(list.files(directory, full.names=TRUE, recursive=TRUE))
    files <- files[!file.info(files)$isdir]
    nFiles <- length(files)

    count <- 0
    identifiers <- character(nFiles)
    
    report(OL$Info, "Reading ", currentSort, " identifiers from ", nFiles, " files")
    for (i in 1:nFiles)
    {
        metadata <- try(newDicomMetadataFromFile(files[i], stopTag=identifierTag), silent=TRUE)
        if (is.null(metadata) || ("try-error" %in% class(metadata)))
        {
            report(OL$Info, "Skipping ", files[i])
            identifiers[i] <- NA_character_
        }
        else
        {
            identifiers[i] <- as.character(metadata$getTagValue(identifierTag[1], identifierTag[2]))
            count <- count + 1
            if (count %% 100 == 0)
                report(OL$Verbose, "Done ", count)
        }
    }

    nDicomFiles <- count
    if (nDicomFiles == 0)
        report(OL$Error, "No readable DICOM files were found")

    uniqueIdentifiers <- na.omit(sort(unique(identifiers)))
    report(OL$Info, "Found ", switch(currentSort,series="series",subject="subjects"), " ", implode(uniqueIdentifiers,", "), "; creating subdirectories")
    
    identifierWidth <- max(nchar(uniqueIdentifiers))
    
    for (id in uniqueIdentifiers)
    {
        matchingFiles <- which(identifiers==id)
        if (length(matchingFiles) > 0)
        {
            metadata <- newDicomMetadataFromFile(files[matchingFiles[1]], stopTag=descriptionTag)
            description <- metadata$getTagValue(descriptionTag[1], descriptionTag[2])
            
            if (currentSort == "series")
            {
                report(OL$Info, "Series ", id, " includes ", length(matchingFiles), " files; description is \"", description, "\"")
                subdirectory <- paste(sprintf(paste("%0",identifierWidth,"d",sep=""),as.integer(id)), gsub("\\W","",description,perl=TRUE), sep="_")
            }
            else if (currentSort == "subject")
            {
                report(OL$Info, "Subject ", id, " includes ", length(matchingFiles), " files")
                subdirectory <- gsub("\\W", "", description, perl=TRUE)
            }
            
            if (!file.exists(file.path(directory, subdirectory)))
                dir.create(file.path(directory, subdirectory))
            
            currentIdFiles <- basename(files[matchingFiles])
            duplicates <- duplicated(currentIdFiles)
            if (any(duplicates))
                currentIdFiles[duplicates] <- paste(currentIdFiles[duplicates], seq_len(sum(duplicates)), sep="_")
            
            from <- files[matchingFiles]
            to <- file.path(directory,subdirectory,currentIdFiles)
            inPlace <- from == to
            success <- file.copy(from[!inPlace], to[!inPlace])
            
            if (!all(success))
                report(OL$Warning, "Not all files copied successfully for ", currentSort, " ", id, " - nothing will be deleted")
            else if (deleteOriginals)
                unlink(files[matchingFiles])
            
            if (length(remainingSorts) > 0)
                sortDicomDirectory(file.path(directory,subdirectory), TRUE, sortOn=remainingSorts)
        }
    }
}

newMriImageMetadataFromDicom <- function (fileName)
{
    fileMetadata <- newDicomMetadataFromFile(fileName)
    invisible (newMriImageMetadataFromDicomMetadata(fileMetadata))
}

newMriImageMetadataFromDicomMetadata <- function (dicom)
{
    if (dicom$getTagValue(0x0008, 0x0060) != "MR")
        flag(OL$Warning, "DICOM file does not contain MR image data")
    
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
    
    # Tags are "Siemens # images in mosaic", "# frames", "Philips # slices"
    slices <- c(dicom$getTagValue(0x0019,0x100a), dicom$getTagValue(0x0028,0x0008), dicom$getTagValue(0x2001,0x1018))
    if (all(is.na(slices)))
        slices <- NULL
    else
        slices <- slices[!is.na(slices)][1]
    
    if (rows != dataRows || columns != dataColumns)
    {
        # Siemens mosaic format
        if (identical(dicom$getTagValue(0x0008,0x0070), "SIEMENS"))
        {
            slicesPerRow <- dataRows / rows
            slicesPerColumn <- dataColumns / columns
            if (is.null(slices))
                slices <- slicesPerRow * slicesPerColumn
            if (slicesPerRow != floor(slicesPerRow) || slicesPerColumn != floor(slicesPerColumn))
            {
                if (rows == dataColumns && columns == dataRows)
                    flag(OL$Info, "Data matrix is transposed relative to acquisition matrix")
                else
                {
                    flag(OL$Warning, "Image dimensions are not a multiple of the acquisition matrix size")
                    slices <- NULL
                }
                
                rows <- dataRows
                columns <- dataColumns
            }
        }
        else
        {
            if (rows == dataColumns && columns == dataRows)
                flag(OL$Info, "Data matrix is transposed relative to acquisition matrix")
            else
                flag(OL$Info, "Image has been upsampled or downsampled after acquisition")

            rows <- dataRows
            columns <- dataColumns
        }
    }
    
    if (is.null(slices) || slices <= 1)
    {
        nDims <- 2
        slices <- NULL
    }
    else
    {
        nDims <- 3
        voxdims <- c(voxdims, dicom$getTagValue(0x0018, 0x0050))
    }
    
    bitsAllocated <- dicom$getTagValue(0x0028, 0x0100)
    if ((bitsAllocated %% 8) != 0)
        report(OL$Error, "Number of bits allocated per pixel doesn't correspond to an integral number of bytes")
    isSigned <- isTRUE(dicom$getTagValue(0x0028, 0x0103) == 1)
    datatype <- list(type="integer", size=bitsAllocated/8, isSigned=isSigned)
    
    metadata <- MriImageMetadata$new(imagedims=c(columns,rows,slices), voxdims=voxdims, voxunit=c("mm","s"), source=dicom$getSource(), datatype=datatype, origin=rep(1,nDims)) 
    invisible (metadata)
}

maskPixels <- function (pixels, metadata)
{
    if (!is.numeric(pixels) || !is.vector(pixels))
        report(OL$Error, "Pixels must be specified as a numeric vector")
    if (!is(metadata, "DicomMetadata"))
        report(OL$Error, "Specified metadata is not a valid DicomMetadata object")
    
    bitsAllocated <- metadata$getTagValue(0x0028, 0x0100)
    bitsStored <- metadata$getTagValue(0x0028, 0x0101)
    highBit <- metadata$getTagValue(0x0028, 0x0102)
    
    if (bitsAllocated == bitsStored)
        return (pixels)
    else if (!is.integer(pixels))
        report(OL$Error, "Pixels must be specified as an integer vector")
    
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
    imageMetadata <- newMriImageMetadataFromDicomMetadata(fileMetadata)
    
    datatype <- imageMetadata$getDataType()
    nPixels <- fileMetadata$getDataLength() / datatype$size
    dims <- imageMetadata$getDimensions()
    nDims <- imageMetadata$getDimensionality()
    endian <- fileMetadata$getEndianness()
    
    connection <- file(fileMetadata$getSource(), "rb")
    seek(connection, where=fileMetadata$getDataOffset())
    pixels <- readBin(connection, "integer", n=nPixels, size=datatype$size, signed=ifelse(datatype$size > 2, TRUE, datatype$isSigned), endian=endian)
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
        if (identical(fileMetadata$getTagValue(0x0008,0x0070), "SIEMENS"))
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
        }
        else
            data <- array(pixels, dim=dims)
        
        if (flipY)
            data <- data[,(dims[2]:1),]
    }
    
    image <- MriImage$new(drop(data), imageMetadata)
    invisible (image)
}

newMriImageFromDicom <- function (fileName)
{
    fileMetadata <- newDicomMetadataFromFile(fileName)
    invisible (newMriImageFromDicomMetadata(fileMetadata))
}

readDiffusionParametersFromMetadata <- function (metadata)
{
    if (!is(metadata, "DicomMetadata"))
        report(OL$Error, "The specified metadata is not a valid DicomMetadata object")
    
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
        report(OL$Error, "The specified path (", dicomDir, ") does not point to a directory")
    
    report(OL$Info, "Looking for DICOM files in directory ", dicomDir)
    files <- expandFileName(list.files(dicomDir, full.names=TRUE, recursive=TRUE))
    files <- files[!file.info(files)$isdir]
    nFiles <- length(files)
    
    dictionary <- NULL
    data("dictionary", package="tractor.base", envir=environment(NULL))

    report(OL$Info, "Reading image information from ", nFiles, " files")
    seriesNumbers <- numeric(0)
    seriesDescriptions <- character(0)
    acquisitionNumbers <- numeric(0)
    imageNumbers <- numeric(0)
    sliceLocations <- numeric(0)
    bValues <- numeric(0)
    bVectors <- matrix(NA, nrow=3, ncol=0)
    images <- list()
    sliceDim <- sliceOrientation <- NULL
    repetitionTime <- 1
    count <- 0
    for (file in files)
    {
        metadata <- newDicomMetadataFromFile(file, dictionary=dictionary)
        if (is.null(metadata))
        {
            report(OL$Info, "Skipping ", file)
            next
        }
        else if (is.null(sliceDim))
        {
            sliceDim <- metadata$getTagValue(0x0018,0x0050)
            sliceOrientation <- metadata$getTagValue(0x0020,0x0037)
            repetitionTime <- metadata$getTagValue(0x0018,0x0080) / 1000
        }

        seriesNumbers <- c(seriesNumbers, metadata$getTagValue(0x0020,0x0011))
        seriesDescriptions <- c(seriesDescriptions, metadata$getTagValue(0x0008,0x103e))
        acquisitionNumbers <- c(acquisitionNumbers, metadata$getTagValue(0x0020,0x0012))
        imageNumbers <- c(imageNumbers, metadata$getTagValue(0x0020,0x0013))
        sliceLocations <- c(sliceLocations, metadata$getTagValue(0x0020,0x1041))
        images <- c(images, list(newMriImageFromDicomMetadata(metadata, flipY=FALSE)))

        if (readDiffusionParams)
        {
            diffusion <- readDiffusionParametersFromMetadata(metadata)
            if (count == 0 && diffusion$defType != "none")
                report(OL$Info, "Attempting to read diffusion parameters using ", diffusion$defType, " DICOM convention")
            bValues <- c(bValues, diffusion$bval)
            bVectors <- cbind(bVectors, diffusion$bvec)
        }

        count <- count + 1
        if (count %% 100 == 0)
            report(OL$Verbose, "Done ", count)
    }

    nDicomFiles <- count
    if (nDicomFiles == 0)
        report(OL$Error, "No readable DICOM files were found")
    
    if (length(unique(seriesDescriptions)) > 1)
        report(OL$Warning, "DICOM directory contains more than one unique series description - merging them may not make sense")
    
    # The sum() function recovers the sign in the sapply() call here
    sliceOrientation <- lapply(list(1:3,4:6), function (i) sliceOrientation[i])
    sliceDirections <- sapply(sliceOrientation, function (x) which(abs(x) == 1) * sum(x))
    
    # Oblique slices case
    if (!is.numeric(sliceDirections) || length(sliceDirections) != 2)
    {
        sliceDirections <- sapply(sliceOrientation, function (x) {
            currentSliceDirection <- which.max(abs(x))
            currentSliceDirection * sign(x[currentSliceDirection])
        })
        
        if (sliceDirections[1] == sliceDirections[2])
            report(OL$Error, "DICOM slice orientation information is complex or nonsensical")
        else
        {
            angles <- sapply(list(1,2), function (i) acos(abs(sliceOrientation[[i]][abs(sliceDirections[i])]) / sqrt(sum(sliceOrientation[[i]]^2))))
            angles <- round(angles / pi * 180, 2)
            report(OL$Warning, "Slices appear to be oblique: rotations from axes are ", implode(angles," and "), " deg")
        }
    }
    
    absoluteSliceDirections <- abs(sliceDirections)
    throughSliceDirection <- setdiff(1:3, absoluteSliceDirections)
    report(OL$Info, "Slice select direction is ", LETTERS[24:26][throughSliceDirection])
    
    # If not TRUE, the data need flipping or transposing
    isSimpleCase <- (all(sliceDirections > 0) && equivalent(setdiff(1:3,throughSliceDirection),absoluteSliceDirections))
    if (!isSimpleCase)
        report(OL$Info, "DICOM files do not use a \"simple\" slice arrangement")

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
        voxelDims <- c(images[[1]]$getVoxelDimensions(), repetitionTime)
    }
    else
    {
        volumePerDicomFile <- FALSE
        nSlices <- length(uniqueSlices)
        nVolumes <- nDicomFiles / nSlices
        if (floor(nVolumes) != nVolumes)
            report(OL$Error, "Number of files (", nDicomFiles, ") is not a multiple of the number of slices detected (", nSlices, ")")
        
        imageDims <- c(NA, NA, NA, nVolumes)
        imageDims[absoluteSliceDirections] <- images[[1]]$getDimensions()
        imageDims[throughSliceDirection] <- nSlices
        
        voxelDims <- c(NA, NA, NA, repetitionTime)
        voxelDims[absoluteSliceDirections] <- images[[1]]$getVoxelDimensions()
        voxelDims[throughSliceDirection] <- sliceDim
    }

    report(OL$Info, "Data set contains ", nVolumes, " volume(s); ", nSlices, " slice(s) per volume")
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
                        if (sliceDirections[1] < 0)
                            loc[[absoluteSliceDirections[1]]] <- imageDims[absoluteSliceDirections[1]]:1
                        if (sliceDirections[2] < 0)
                            loc[[absoluteSliceDirections[2]]] <- imageDims[absoluteSliceDirections[2]]:1
                        loc$t <- volume

                        sliceData <- images[[slicePos]]$getData()
                        if (absoluteSliceDirections[1] > absoluteSliceDirections[2])
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
        report(OL$Info, "Slice location decreases between consecutive images - inverting slice order")
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
    
    returnValue <- list(image=image, seriesDescriptions=unique(seriesDescriptions))
    if (readDiffusionParams)
        returnValue <- c(returnValue, list(bValues=volumeBValues, bVectors=volumeBVectors))
    
    invisible (returnValue)
}

newDicomMetadataFromFile <- function (fileName, checkFormat = TRUE, dictionary = NULL, stopTag = NULL)
{
    fileName <- expandFileName(fileName)
    
    if (!file.exists(fileName))
        report(OL$Error, "DICOM file ", fileName, " not found")
    
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
        str <- rawToChar(stripNul(readBin(connection, "raw", n=4)))
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
    type <- rawToChar(stripNul(readBin(connection, "raw", n=2)))
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
        duplicateTags <- FALSE
        
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
                type <- rawToChar(stripNul(readBin(connection, "raw", n=2)))
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
            
            length <- readBin(connection, "integer", n=1, size=lengthSize, signed=ifelse(lengthSize > 2, TRUE, FALSE), endian=endian)
            
            report(OL$Debug, "Group ", sprintf("0x%04x",currentGroup), ", element ", sprintf("0x%04x",currentElement), ", type ", type, ", length ", length, ifelse(sequenceLevel>0," (in sequence)",""))
            
            if (any(c("OX","OW","OB","UN") == type) || (type == "SQ" && sequenceLevel > 0))
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
            else if (any(groups==currentGroup & elements==currentElement))
            {
                duplicateTags <- TRUE
                if (length > 0)
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
                    value <- readBin(connection, "integer", n=nValues, size=size, signed=ifelse(size > 2, TRUE, .Dicom$nonCharTypes$isSigned[loc]), endian=endian)
                else
                    value <- readBin(connection, "double", n=nValues, size=size, endian=endian)
                
                if (nValues > 1)
                    values <- c(values, implode(value,sep="\\"))
                else
                    values <- c(values, as.character(value))
            }
            else
                values <- c(values, rawToChar(stripNul(readBin(connection, "raw", n=length))))
            
            if (!is.null(stopTag) && currentGroup == stopTag[1] && currentElement == stopTag[2])
            {
                dataOffset <- NA
                dataLength <- NA
                break
            }
        }
        
        close(connection)

        if (is.null(dataOffset))
            invisible (NULL)
        else
        {
            if (duplicateTags)
                flag(OL$Warning, "Duplicated DICOM tags detected - only the first value will be kept")
            
            tags <- data.frame(groups=groups, elements=elements, types=types, values=values, stringsAsFactors=FALSE)
            invisible (DicomMetadata$new(source=fileName, tags=tags, tagOffset=as.integer(tagOffset), dataOffset=as.integer(dataOffset), dataLength=as.integer(dataLength), explicitTypes=explicitTypes, endian=endian))
        }
    }
    else
    {
        close(connection)
        invisible (NULL)
    }
}
