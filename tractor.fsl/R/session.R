.MriSession <- function (.directory)
{
    .directory <- expandFileName(.directory)
    if (!file.exists(.directory))
        output(OL$Error, "Session directory does not exist")
    
    self <- list(
        getBaseDirectory = function () { return (.directory) },
        
        getBedpostDirectory = function () { return (file.path(.directory, "combined", "fdt.bedpostX")) },
        
        getImageByType = function (type, fibrePopulation = 1)
        {
            fileName <- self$getImageFileNameByType(type, fibrePopulation)
            if (type %in% c("fa","md") && !imageFileExists(fileName))
                runDtifitWithSession(self)
            return (newMriImageFromFile(fileName))
        },
        
        getImageFileNameByType = function (type, fibrePopulation = 1)
        {
            if (type == "avf")
                return (file.path(self$getBedpostDirectory(), paste("mean_f",fibrePopulation,"samples",sep="")))
            else if (type == "t2")
                return (file.path(self$getPreBedpostDirectory(), "nodif_brain"))
            else if (type == "mask")
                return (file.path(self$getPreBedpostDirectory(), "nodif_brain_mask"))
            else if (type == "theta")
                return (file.path(self$getBedpostDirectory(), paste("merged_th",fibrePopulation,"samples",sep="")))
            else if (type == "phi")
                return (file.path(self$getBedpostDirectory(), paste("merged_ph",fibrePopulation,"samples",sep="")))
            else if (type == "fa")
                return (file.path(self$getPreBedpostDirectory(), "dti_FA"))
            else if (type == "md")
                return (file.path(self$getPreBedpostDirectory(), "dti_MD"))
            else
                output(OL$Error, "Unknown file type (", type, ") specified")
        },
        
        getObjectDirectory = function ()
        {
            objectDir <- file.path(.directory, "combined", "objects")
            if (!file.exists(objectDir))
                dir.create(objectDir)
            return (objectDir)
        },
        
        getObjectFileName = function (object)
        {
            return (paste(self$getObjectDirectory(), "/", object, ".Rdata", sep=""))
        },
        
        getPreBedpostDirectory = function ()
        {
            preBedpostDir <- file.path(.directory, "combined", "fdt")
            if (!file.exists(preBedpostDir))
                dir.create(preBedpostDir)
            return (preBedpostDir)
        },
        
        getProbtrackDirectory = function ()
        {
            probtrackDir <- file.path(.directory, "combined", "fdt.track")
            if (!file.exists(probtrackDir))
                dir.create(probtrackDir)
            return (probtrackDir)
        },
        
        getWorkingDirectory = function () { return (file.path(.directory, "combined")) },
        
        isPreprocessed = function () { return (imageFileExists(self$getImageFileNameByType("avf"))) },
        
        nFibres = function ()
        {
            if (!self$isPreprocessed())
                return (NA)
            
            # This will give the wrong answer if more than 3 populations were
            # used (but I think Behrens advises against that anyway)
            for (i in 1:4)
            {
                if (!imageFileExists(self$getImageFileNameByType("avf",fibrePopulation=i)))
                    break
            }
            return (i-1)
        }
    )
    
    class(self) <- c("session.mri", "list.object", "list")
    invisible (self)
}

isMriSession <- function (object)
{
    return ("session.mri" %in% class(object))
}

newSessionFromDirectory <- function (directory, createFiles = FALSE)
{
    session <- .MriSession(directory)
    if (createFiles)
        createFilesForSession(session)
    invisible (session)
}

createFilesForSession <- function (session)
{
    workingDir <- session$getWorkingDirectory()
    if (file.exists(workingDir))
    {
        ans <- output(OL$Question, "Internal directory ", workingDir, " exists. This operation will DESTROY it. Continue? [yn]")
        if (tolower(ans) != "y")
            return (invisible(NULL))
        else
            unlink(workingDir, recursive=TRUE)
    }
    
    files <- expandFileName(list.files(session$getBaseDirectory(), full.names=TRUE, recursive=TRUE))
    files <- files[!file.info(files)$isdir]
    nFiles <- length(files)
    
    output(OL$Info, "Reading image information from ", nFiles, " files")
    seriesNumbers <- numeric(0)
    acquisitionNumbers <- numeric(0)
    imageNumbers <- numeric(0)
    sliceLocations <- numeric(0)
    bValues <- numeric(0)
    bVectors <- matrix(NA, nrow=3, ncol=0)
    images <- list()
    zVoxDim <- NULL
    count <- 0
    for (file in files)
    {
        metadata <- newDicomMetadataFromFile(file)
        if (is.null(metadata))
        {
            output(OL$Info, "Skipping ", file)
            next
        }
        else if (is.null(zVoxDim))
            zVoxDim <- metadata$getTagValue(0x0018,0x0050)
        
        seriesNumbers <- c(seriesNumbers, metadata$getTagValue(0x0020,0x0011))
        acquisitionNumbers <- c(acquisitionNumbers, metadata$getTagValue(0x0020,0x0012))
        imageNumbers <- c(imageNumbers, metadata$getTagValue(0x0020,0x0013))
        sliceLocations <- c(sliceLocations, metadata$getTagValue(0x0020,0x1041))
        images <- c(images, list(newMriImageFromDicomMetadata(metadata)))
        
        diffusion <- readDiffusionParametersFromMetadata(metadata)
        if (count == 0 && diffusion$defType != "none")
            output(OL$Info, "Attempting to read diffusion parameters using ", diffusion$defType, " DICOM convention")
        bValues <- c(bValues, diffusion$bval)
        bVectors <- cbind(bVectors, diffusion$bvec)

        count <- count + 1
        if (count %% 100 == 0)
            output(OL$Verbose, "Done ", count)
    }

    nDicomFiles <- count
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
        imageDims <- c(images[[1]]$getDimensions(), nSlices, nVolumes)
        voxelDims <- c(images[[1]]$getVoxelDimensions(), zVoxDim, 1)
    }
    
    output(OL$Info, "Data set contains ", nVolumes, " volumes; ", nSlices, " slices per volume")
    data <- array(NA, dim=imageDims)

    volumeBValues <- rep(NA, nVolumes)
    volumeBVectors <- matrix(NA, nrow=3, ncol=nVolumes)

    # This is meant to be zero (so that the modulo arithmetic works)
    index <- 0
    for (s in uniqueSeries)
    {
        for (a in uniqueAcquisitions)
        {
            for (i in uniqueImages)
            {
                sliceLoc <- which(seriesNumbers==s & acquisitionNumbers==a & imageNumbers==i)
                
                if (volumePerDicomFile)
                {
                    volume <- index + 1
                    data[,,,volume] <- images[[sliceLoc]]$getData()
                }
                else
                {
                    slice <- (index %% nSlices) + 1
                    volume <- (index %/% nSlices) + 1
                    data[,,slice,volume] <- images[[sliceLoc]]$getData()
                }
                
                if (is.na(volumeBValues[volume]))
                {
                    volumeBValues[volume] <- bValues[sliceLoc]
                    volumeBVectors[,volume] <- bVectors[,sliceLoc]
                }
                
                index <- index + 1
            }
        }
    }

    imageMetadata <- newMriImageMetadataFromTemplate(images[[1]]$getMetadata(), imageDims=imageDims, voxelDims=voxelDims, origin=rep(1,length(imageDims)))
    rm(images)
    image <- newMriImageWithData(data, imageMetadata)
    
    dir.create(workingDir)
    targetDir <- session$getPreBedpostDirectory()
    writeMriImageToFile(image, file.path(targetDir,"basic"))
    image$summarise()
    rm(image)
    
    if (length(which(is.na(bValues))) != 0)
        output(OL$Info, "Diffusion b-values could not be found in the DICOM files; you need to create a bvals file manually")
    else
        write.table(matrix(volumeBValues,nrow=1), file.path(targetDir,"bvals"), row.names=FALSE, col.names=FALSE)
    
    if (length(which(is.na(bVectors))) != 0)
        output(OL$Info, "Diffusion gradient vectors could not be found in the DICOM files; you need to create a bvecs file manually")
    else
        write.table(volumeBVectors, file.path(targetDir,"bvecs"), row.names=FALSE, col.names=FALSE)
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

flipGradientVectorsForSession <- function (session, axes)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    fileName <- file.path(session$getPreBedpostDirectory(), "bvecs")
    bvecs <- as.matrix(read.table(fileName))
    bvecs[axes,] <- (-bvecs[axes,])
    write.table(bvecs, fileName, row.names=FALSE, col.names=FALSE)
}
