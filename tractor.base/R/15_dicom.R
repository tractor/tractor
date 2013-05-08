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
    
    sortOn <- match.arg(sortOn, c("series","subject","date"), several.ok=TRUE)
    currentSort <- sortOn[1]
    remainingSorts <- sortOn[-1]
    identifierTag <- switch(currentSort, series=c(0x0020,0x0011), subject=c(0x0010,0x0010), date=c(0x0008,0x0020))
    descriptionTag <- switch(currentSort, series=c(0x0008,0x103e), subject=c(0x0010,0x0010), date=c(0x0008,0x0020))
    
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
    report(OL$Info, "Found ", switch(currentSort,series="series",subject="subjects",date="dates"), " ", implode(uniqueIdentifiers,", "), "; creating subdirectories")
    
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
            else if (currentSort == "date")
            {
                report(OL$Info, "Date ", id, " includes ", length(matchingFiles), " files")
                subdirectory <- as.character(description)
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
                unlink(from[!inPlace])
            
            if (length(remainingSorts) > 0)
                sortDicomDirectory(file.path(directory,subdirectory), TRUE, sortOn=remainingSorts)
        }
    }
}

newMriImageFromDicomMetadata <- function (metadata, flipY = TRUE, untileMosaics = TRUE, metadataOnly = FALSE)
{
    if (metadata$getTagValue(0x0008, 0x0060) != "MR")
        flag(OL$Warning, "DICOM file does not contain MR image data")
    
    acquisitionMatrix <- metadata$getTagValue(0x0018, 0x1310)
    rows <- max(acquisitionMatrix[1], acquisitionMatrix[3])
    columns <- max(acquisitionMatrix[2], acquisitionMatrix[4])
    dataRows <- metadata$getTagValue(0x0028, 0x0010)
    dataColumns <- metadata$getTagValue(0x0028, 0x0011)
    voxelDims <- metadata$getTagValue(0x0028, 0x0030)
    endian <- metadata$getEndianness()
    mosaic <- FALSE
    
    if (is.na(rows))
        rows <- dataRows
    if (is.na(columns))
        columns <- dataColumns
    
    # Tags are "Siemens # images in mosaic", "# frames", "Philips # slices"
    slices <- c(metadata$getTagValue(0x0019,0x100a), metadata$getTagValue(0x0028,0x0008), metadata$getTagValue(0x2001,0x1018))
    if (all(is.na(slices)))
        slices <- NULL
    else
        slices <- slices[!is.na(slices)][1]
    
    if (rows != dataRows || columns != dataColumns)
    {
        # Siemens mosaic format
        if (identical(metadata$getTagValue(0x0008,0x0070), "SIEMENS") && untileMosaics)
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
            mosaic <- TRUE
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
        voxelDims <- c(voxelDims, metadata$getTagValue(0x0018, 0x0050))
    }
    
    dims <- c(columns, rows, slices)
    
    if (metadataOnly)
        data <- NULL
    else
    {
        bitsAllocated <- metadata$getTagValue(0x0028, 0x0100)
        if ((bitsAllocated %% 8) != 0)
            report(OL$Error, "Number of bits allocated per pixel doesn't correspond to an integral number of bytes")
        bytesPerPixel <- bitsAllocated / 8
        isSigned <- isTRUE(metadata$getTagValue(0x0028, 0x0103) == 1)
        
        connection <- file(metadata$getSource(), "rb")
        seek(connection, where=metadata$getDataOffset())
        pixels <- readBin(connection, "integer", n=metadata$getDataLength()/bytesPerPixel, size=bytesPerPixel, signed=ifelse(bytesPerPixel > 2, TRUE, isSigned), endian=endian)
        pixels <- maskPixels(pixels, metadata)
        close(connection)
    
        if (nDims == 2)
        {
            data <- array(pixels, dim=dims)
            if (flipY)
                data <- data[,(dims[2]:1)]
        }
        else if (nDims == 3)
        {
            if (mosaic)
            {
                # Handle Siemens mosaic images, which encapsulate a whole 3D image in a single-frame DICOM file
                mosaicDims <- c(metadata$getTagValue(0x0028, 0x0010), metadata$getTagValue(0x0028, 0x0011))
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
    }
    
    if (mosaic)
        image <- MriImage$new(imageDims=dims, voxelDims=voxelDims, voxelDimUnits=c("mm","s"), source=metadata$getSource(), origin=rep(1,nDims), data=data, tags=list(keys=".mosaicDims",values=mosaicDims))
    else
        image <- MriImage$new(imageDims=dims, voxelDims=voxelDims, voxelDimUnits=c("mm","s"), source=metadata$getSource(), origin=rep(1,nDims), data=data, tags=list())
    
    invisible (image)
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

newMriImageFromDicom <- function (fileName, ...)
{
    metadata <- newDicomMetadataFromFile(fileName)
    invisible (newMriImageFromDicomMetadata(metadata, ...))
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

newMriImageFromDicomDirectory <- function (dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
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
    info <- data.frame(seriesNumber=numeric(nFiles), seriesDescription=character(nFiles), acquisitionNumber=numeric(nFiles), imageNumber=numeric(nFiles), sliceLocation=numeric(nFiles), stringsAsFactors=FALSE)
    images <- vector("list", nFiles)
    if (readDiffusionParams)
    {
        bValues <- numeric(nFiles)
        bVectors <- matrix(NA, nrow=3, ncol=nFiles)
    }
    
    valid <- rep(TRUE, nFiles)
    seenValidFile <- FALSE
    sliceDim <- sliceOrientation <- throughSliceOrientation <- NULL
    repetitionTime <- 1
    count <- 0
    for (i in seq_along(files))
    {
        metadata <- newDicomMetadataFromFile(files[i], dictionary=dictionary)
        if (is.null(metadata))
        {
            # Not a DICOM file - skip it
            report(OL$Verbose, "Skipping ", files[i])
            valid[i] <- FALSE
            next
        }
        else if (!seenValidFile)
        {
            # Read slice dimensions and orientation once - these are assumed not to vary across files
            sliceDim <- metadata$getTagValue(0x0018,0x0050)
            sliceOrientation <- metadata$getTagValue(0x0020,0x0037)
            
            # Calculate through-slice orientation (in LPS convention)
            throughSliceOrientation <- vectorCrossProduct(sliceOrientation[1:3], sliceOrientation[4:6])
            
            # Read TR for temporal dimension
            repetitionTime <- metadata$getTagValue(0x0018,0x0080) / 1000
        }
        
        # Read in metadata specific to this file
        info$seriesNumber[i] <- metadata$getTagValue(0x0020,0x0011)
        info$seriesDescription[i] <- metadata$getTagValue(0x0008,0x103e)
        info$acquisitionNumber[i] <- metadata$getTagValue(0x0020,0x0012)
        info$imageNumber[i] <- metadata$getTagValue(0x0020,0x0013)
        imagePosition <- metadata$getTagValue(0x0020,0x0032)
        info$sliceLocation[i] <- imagePosition %*% throughSliceOrientation
        
        # Read in the pixel data and usual MriImage metadata
        images[[i]] <- newMriImageFromDicomMetadata(metadata, flipY=FALSE, untileMosaics=untileMosaics)
        
        # Check for diffusion metadata if requested
        if (readDiffusionParams)
        {
            diffusion <- readDiffusionParametersFromMetadata(metadata)
            if (!seenValidFile && diffusion$defType != "none")
                report(OL$Info, "Attempting to read diffusion parameters using ", diffusion$defType, " DICOM convention")
            bValues[i] <- diffusion$bval
            bVectors[,i] <- diffusion$bvec
        }
        
        if (!seenValidFile)
            seenValidFile <- TRUE
        
        if (i %% 100 == 0)
            report(OL$Verbose, "Done ", count)
    }
    
    nDicomFiles <- sum(valid)
    if (nDicomFiles == 0)
        report(OL$Error, "No readable DICOM files were found")
    
    info <- info[valid,]
    images <- images[valid]
    if (readDiffusionParams)
    {
        bValues <- bValues[valid]
        bVectors <- bVectors[,valid]
    }
    
    if (length(unique(info$seriesDescription)) > 1)
        report(OL$Warning, "DICOM directory contains more than one unique series description - merging them may not make sense")
    
    # Slice orientations are the in-plane X and Y and through plane direction vectors, in that order
    # Slice directions are 1, 2 and 3 for X, Y and Z in the LPS reference system; sign indicates flip
    # NB: The sum() function recovers the sign in the sapply() call here
    sliceOrientation <- list(sliceOrientation[1:3], sliceOrientation[4:6], throughSliceOrientation)
    sliceDirections <- sapply(sliceOrientation, function (x) round(which(abs(x) == 1) * sum(x)))
    
    # Oblique slices case: look for the closest reference orientations and warn
    if (!is.numeric(sliceDirections) || length(sliceDirections) != 3)
    {
        sliceDirections <- sapply(sliceOrientation, function (x) {
            currentSliceDirection <- which.max(abs(x))
            currentSliceDirection * sign(x[currentSliceDirection])
        })
        
        if (sliceDirections[1] == sliceDirections[2])
            report(OL$Error, "DICOM slice orientation information is complex or nonsensical")
        else
        {
            angles <- sapply(1:2, function (i) acos(abs(sliceOrientation[[i]][abs(sliceDirections[i])]) / sqrt(sum(sliceOrientation[[i]]^2))))
            angles <- round(angles / pi * 180, 2)
            report(OL$Warning, "Slices appear to be oblique: rotations from axes are ", implode(angles," and "), " deg")
        }
    }
    
    report(OL$Info, "Image orientation is ", implode(c("I","A","R","","L","P","S")[sliceDirections+4],sep=""))
    absoluteSliceDirections <- abs(sliceDirections)
    
    # Find all unique slice positions and find the index corresponding to each image
    uniqueSlices <- sort(unique(info$sliceLocation))
    info$sliceIndex <- match(info$sliceLocation, uniqueSlices)
    
    # Is there a volume stored in each DICOM file? Is the image a mosaic?
    volumePerDicomFile <- (images[[1]]$getDimensionality() == 3)
    mosaic <- !is.na(images[[1]]$getTag(".mosaicDims"))
    
    if (!volumePerDicomFile && length(uniqueSlices) < 2)
        report(OL$Error, "Reading a single 2D image from DICOM is not supported at present")
    
    # Work out the image and voxel dimensions of the unpermuted image
    if (volumePerDicomFile)
    {
        nSlices <- images[[1]]$getDimensions()[3]
        nVolumes <- nDicomFiles
        imageDims <- c(images[[1]]$getDimensions(), nVolumes)
        voxelDims <- c(images[[1]]$getVoxelDimensions(), repetitionTime)
    }
    else
    {
        if (!equivalent((uniqueSlices[2] - uniqueSlices[1]) / sliceDim, 1, tolerance=1e-3))
        {
            report(OL$Warning, "Slice thickness and slice separation do not match - the latter will be used")
            sliceDim <- uniqueSlices[2] - uniqueSlices[1]
        }
        
        nSlices <- length(uniqueSlices)
        nVolumes <- nDicomFiles / nSlices
        if (floor(nVolumes) != nVolumes)
            report(OL$Error, "Number of files (", nDicomFiles, ") is not a multiple of the number of slices detected (", nSlices, ")")
        
        # Dimensions of initial image, to be permuted later if necessary
        imageDims <- c(images[[1]]$getDimensions(), nSlices, nVolumes)
        voxelDims <- c(images[[1]]$getVoxelDimensions(), sliceDim, repetitionTime)
    }

    report(OL$Info, "Data set contains ", nVolumes, " volume(s); ", nSlices, " slice(s) per volume")
    data <- array(NA, dim=imageDims)

    # Sort by series number, then acquisition number, then image number
    sortOrder <- order(info$seriesNumber, info$acquisitionNumber, info$imageNumber)
    info <- info[sortOrder,]
    images <- images[sortOrder]
    
    if (readDiffusionParams)
    {
        bValues <- bValues[sortOrder]
        bVectors <- bVectors[,sortOrder]
        
        # Initialisation
        volumeBValues <- rep(NA, nVolumes)
        volumeBVectors <- matrix(NA, nrow=3, ncol=nVolumes)
    }
    
    # Insert data into the appropriate places
    for (i in 1:nrow(info))
    {
        if (volumePerDicomFile)
        {
            volume <- i
            data[,,,volume] <- images[[i]]$getData()
        }
        else
        {
            slice <- info$sliceIndex[i]
            volume <- ((i-1) %/% nSlices) + 1
            data[,,slice,volume] <- images[[i]]$getData()
        }
        
        # Insert diffusion parameters once per volume
        if (readDiffusionParams && is.na(volumeBValues[volume]))
        {
            volumeBValues[volume] <- bValues[i]
            volumeBVectors[,volume] <- bVectors[,i]
        }
    }

    if (mosaic)
    {
        # NB: This assumes that the slices in the mosaic are axial, with the usual DICOM LPS convention
        # This assumption has been met by images seen to date, but may not always be
        data <- data[,imageDims[2]:1,,,drop=TRUE]
        ordering <- c(1,-1,1)
        
        # The image position in plane is stored wrongly for mosaic images, so we need to correct it
        imagePosition[1:2] <- imagePosition[1:2] + abs(voxelDims[1:2]) * ((as.numeric(images[[1]]$getTag(".mosaicDims"))-imageDims[1:2]) / 2)
    }
    else
    {
        # Permute the data dimensions as required
        dimPermutation <- c(match(1:3,absoluteSliceDirections), 4)
        if (!equivalent(dimPermutation, 1:4))
        {
            data <- aperm(data, dimPermutation)
            imageDims <- imageDims[dimPermutation]
            voxelDims <- abs(voxelDims[dimPermutation]) * c(-1,1,1,1)
        }
        
        # Check for any flips required to achieve LAS orientation
        ordering <- sign(sliceDirections[dimPermutation])[1:3] * c(1,-1,1)
        orderX <- (if (ordering[1] == 1) seq_len(imageDims[1]) else rev(seq_len(imageDims[1])))
        orderY <- (if (ordering[2] == 1) seq_len(imageDims[2]) else rev(seq_len(imageDims[2])))
        orderZ <- (if (ordering[3] == 1) seq_len(imageDims[3]) else rev(seq_len(imageDims[3])))
        data <- data[orderX,orderY,orderZ,,drop=TRUE]
    }
    
    dimsToKeep <- which(imageDims > 1)
    imageDims <- imageDims[dimsToKeep]
    voxelDims <- voxelDims[dimsToKeep]
    
    # Set the image position in the through-slice direction to the location of the first slice
    imagePosition[absoluteSliceDirections[3]] <- info$sliceLocation[info$sliceIndex == 1][1]
    
    # Invert position for Y direction due to switch to LAS convention
    imagePosition[2] <- -imagePosition[2]
    
    # Origin is 0 for temporal dimensions
    # For other dimensions, use the image position (which is the centre of the first voxel stored)
    origin <- rep(0, length(dimsToKeep))
    origin[1:3] <- 1 - ordering[1:3] * round(imagePosition/voxelDims[1:3],2)
    origin[1:3] <- ifelse(ordering[1:3] == c(1,1,1), origin[1:3], imageDims[1:3]-origin[1:3]+1)
    
    # Create the final image
    image <- newMriImageWithData(data, templateImage=images[[1]], imageDims=imageDims, voxelDims=voxelDims, origin=origin, tags=list())
    
    returnValue <- list(image=image, seriesDescriptions=unique(info$seriesDescription))
    if (readDiffusionParams)
        returnValue <- c(returnValue, list(bValues=volumeBValues, bVectors=volumeBVectors))
    
    invisible (returnValue)
}

newDicomMetadataFromFile <- function (fileName, checkFormat = TRUE, dictionary = NULL, stopTag = NULL, ignoreTransferSyntax = FALSE)
{
    fileName <- expandFileName(fileName)
    
    if (!file.exists(fileName))
        report(OL$Error, "DICOM file ", fileName, " not found")
    
    # DICOM is sufficiently complicated that this can really only be interpreted to mean "probably" or "probably not"
    isDicomFile <- !checkFormat
    endian <- "little"
    explicitTypes <- TRUE
    deferredTransferSyntax <- NULL
    tagOffset <- 0
    dataOffset <- NULL
    
    if (is.null(dictionary))
        data("dictionary", envir=environment(NULL))
    dictionary$type <- as.vector(dictionary$type)
    typeCol <- which(colnames(dictionary) == "type")
    connection <- file(fileName, "rb")
    on.exit(close(connection))
    
    if (checkFormat)
    {
        # A well-formed DICOM file should contain a magic number at offset 128
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
    
    # Read the first group number
    group <- readBin(connection, "integer", n=1, size=2, signed=FALSE, endian=endian)
    
    # Some GE files have no magic number and no group 2, starting with group 8 instead
    if (checkFormat && !isDicomFile)
    {
        if (isTRUE(group == 0x0008))
            isDicomFile <- TRUE
        else if (isTRUE(group == 0x0800))
            isDicomFile <- TRUE
    }
    
    # Assume that the first group shouldn't be huge, to guess the endianness
    if (isTRUE(group > 0x00ff))
        endian <- setdiff(c("big","little"), endian)
    
    # Pretty fragile, this, since it assumes we know the type of the first tag
    # Seems to work in practice, but in theory we can assume implicit little-endian
    # after group 2 if there is no transfer syntax specified
    seek(connection, where=2, origin="current")
    type <- rawToChar(stripNul(readBin(connection, "raw", n=2)))
    if (isTRUE(type == "UL"))
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
            
            # Group 2 should always be explicit little-endian; the transfer syntax applies after that
            if (!ignoreTransferSyntax && !is.null(deferredTransferSyntax) && currentGroup > 2)
            {
                endian <- .Dicom$transferSyntaxes[[deferredTransferSyntax]]$endian
                explicitTypes <- .Dicom$transferSyntaxes[[deferredTransferSyntax]]$explicitTypes
                deferredTransferSyntax <- NULL
            }
            
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
                else if (nValues == 0)
                    values <- c(values, "")
                else
                    values <- c(values, as.character(value))
            }
            else
                values <- c(values, rawToChar(stripNul(readBin(connection, "raw", n=length))))
            
            if (!ignoreTransferSyntax && currentGroup == 0x0002 && currentElement == 0x0010)
            {
                deferredTransferSyntax <- values[length(values)]
                if (!(deferredTransferSyntax %in% names(.Dicom$transferSyntaxes)))
                    report(OL$Error, "Transfer syntax ", deferredTransferSyntax, " is not supported")
            }
            
            if (!is.null(stopTag) && currentGroup == stopTag[1] && currentElement == stopTag[2])
            {
                dataOffset <- NA
                dataLength <- NA
                break
            }
        }
        
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
        invisible (NULL)
}
