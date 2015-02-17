sortDicomDirectory <- function (directory, deleteOriginals = FALSE, sortOn = "series", useSeriesTime = FALSE)
{
    if (!file.exists(directory) || !file.info(directory)$isdir)
        report(OL$Error, "Specified path (", directory, ") does not exist or does not point to a directory")
    
    sortOn <- match.arg(sortOn, c("series","subject","date"), several.ok=TRUE)
    currentSort <- sortOn[1]
    remainingSorts <- sortOn[-1]
    identifierTag <- switch(currentSort, series=(if (useSeriesTime) c(0x0008,0x0031) else c(0x0020,0x0011)), subject=c(0x0010,0x0010), date=c(0x0008,0x0020))
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
            if (useSeriesTime && currentSort == "series")
                identifiers[i] <- sub("\\..+$", "", identifiers[i], perl=TRUE)
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

readDicomDirectory <- function (dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
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
        images[[i]] <- readImageParametersFromMetadata(metadata, untileMosaics=untileMosaics)
        
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
    volumePerDicomFile <- (length(images[[1]]$imageDims) == 3)
    mosaic <- images[[1]]$mosaic
    
    if (!volumePerDicomFile && length(uniqueSlices) < 2)
        report(OL$Error, "Reading a single 2D image from DICOM is not supported at present")
    
    # Work out the image and voxel dimensions of the unpermuted image
    if (volumePerDicomFile)
    {
        nSlices <- images[[1]]$imageDims[3]
        nVolumes <- nDicomFiles
        imageDims <- c(images[[1]]$imageDims, nVolumes)
        
        # Some mosaic files store the per-slice TR, but we want the per-volume TR
        # Assume that a TR below 500 ms must be per-slice
        if (mosaic && repetitionTime < 0.5)
            repetitionTime <- repetitionTime * nSlices
        voxelDims <- c(images[[1]]$voxelDims, repetitionTime)
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
        imageDims <- c(images[[1]]$imageDims, nSlices, nVolumes)
        voxelDims <- c(images[[1]]$voxelDims, sliceDim, repetitionTime)
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
            data[,,,volume] <- images[[i]]$data
        }
        else
        {
            slice <- info$sliceIndex[i]
            volume <- ((i-1) %/% nSlices) + 1
            data[,,slice,volume] <- images[[i]]$data
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
        # The image position in plane is stored wrongly for mosaic images, so we need to correct it
        inPlaneDims <- absoluteSliceDirections[1:2]
        imagePosition[inPlaneDims] <- imagePosition[inPlaneDims] + abs(voxelDims[inPlaneDims]) * ((images[[1]]$mosaicDims-imageDims[inPlaneDims]) / 2)
    }
    
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
    origin[1:3] <- 1 - ordering[1:3] * (imagePosition / voxelDims[1:3])
    origin[1:3] <- ifelse(ordering[1:3] == c(1,1,1), origin[1:3], imageDims[1:3]-origin[1:3]+1)
    
    # Create the final image
    image <- newMriImageWithData(data, imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=c("mm","s"), origin=origin, tags=list())
    
    returnValue <- list(image=image, seriesDescriptions=unique(info$seriesDescription))
    if (readDiffusionParams)
        returnValue <- c(returnValue, list(bValues=volumeBValues, bVectors=volumeBVectors))
    
    invisible (returnValue)
}

newMriImageFromDicomDirectory <- function (dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
{
    readDicomDirectory(dicomDir, readDiffusionParams, untileMosaics)
}

readImageParametersFromMetadata <- function (metadata, untileMosaics = TRUE, metadataOnly = FALSE)
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
            mosaicDims <- c(metadata$getTagValue(0x0028, 0x0010), metadata$getTagValue(0x0028, 0x0011))
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
        
        # Apply slope and intercept, if defined
        if (!is.na(metadata$getTagValue(0x0028, 0x1053)))
            pixels <- pixels * metadata$getTagValue(0x0028, 0x1053)
        if (!is.na(metadata$getTagValue(0x0028, 0x1052)))
            pixels <- pixels + metadata$getTagValue(0x0028, 0x1052)
    
        if (nDims == 2)
            data <- array(pixels, dim=dims)
        else if (nDims == 3)
        {
            if (mosaic)
            {
                # Handle Siemens mosaic images, which encapsulate a whole 3D image in a single-frame DICOM file
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
        }
    }
    
    returnValue <- list(imageDims=dims, voxelDims=voxelDims, data=data, mosaic=mosaic)
    if (mosaic)
        returnValue$mosaicDims <- mosaicDims
    
    return (returnValue)
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
