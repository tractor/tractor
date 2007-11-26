getParametersForFileType <- function (fileType = NA, format = NA, singleFile = NA, gzipped = NA, errorIfInvalid = TRUE)
{
    if (is.character(fileType))
        typeIndex <- which(.FileTypes$typeNames == toupper(fileType))
    else
        typeIndex <- which(.FileTypes$formatNames == format & .FileTypes$singleFile == singleFile & .FileTypes$gzipped == gzipped)
    
    if (length(typeIndex) != 1)
    {
        if (errorIfInvalid)
            output(OL$Error, "Specified file type information is incomplete or invalid")
        else
            return (NULL)
    }
    
    parameters <- list(name=.FileTypes$typeNames[typeIndex],
                       format=.FileTypes$formatNames[typeIndex],
                       singleFile=.FileTypes$singleFile[typeIndex],
                       gzipped=.FileTypes$gzipped[typeIndex],
                       headerSuffix=.FileTypes$headerSuffixes[typeIndex],
                       imageSuffix=.FileTypes$imageSuffixes[typeIndex])
    
    return (parameters)
}

identifyImageFileNames <- function (fileName, fileType = NULL, errorIfMissing = TRUE)
{
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    fileName <- expandFileName(fileName)
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    headersExist <- intersect(unique(.FileTypes$headerSuffixes), suffixes[exist])
    imagesExist <- intersect(unique(.FileTypes$imageSuffixes), suffixes[exist])
    
    if (length(headersExist) != 1 || length(imagesExist) != 1)
    {
        if (errorIfMissing)
            output(OL$Error, "File does not exist or multiple compatible files exist")
        else
            return (NULL)
    }
    
    typeIndices <- which(.FileTypes$headerSuffixes == headersExist &
                         .FileTypes$imageSuffixes == imagesExist)
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, headersExist)
    imageFile <- ensureFileSuffix(fileStem, imagesExist)
    
    # ANALYZE and NIFTI_PAIR file types use the same filename suffixes
    if (length(typeIndices) == 1)
        format <- .FileTypes$format[typeIndices]
    else if (!is.null(fileType))
        format <- (getParametersForFileType(fileType, errorIfInvalid=TRUE))$format
    else
        format <- ifelse(hasNiftiMagicString(headerFile), "Nifti", "Analyze")
    
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile, format=format)
    return (fileNames)
}

removeImageFilesWithName <- function (fileName)
{
    fileName <- expandFileName(fileName)
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)    
    files <- ensureFileSuffix(fileName, suffixes)
    unlink(files)
}

newMriImageMetadataFromFile <- function (fileName, fileType = NULL)
{
    fileNames <- identifyImageFileNames(fileName, fileType)
    
    if (fileNames$format == "Analyze")
        metadata <- newMriImageMetadataFromAnalyze(fileNames)
    else if (fileNames$format == "Nifti")
        metadata <- newMriImageMetadataFromNifti(fileNames)
    
    invisible (metadata)
}

newMriImageFromFile <- function (fileName, fileType = NULL)
{
    fileNames <- identifyImageFileNames(fileName, fileType)
    
    if (fileNames$format == "Analyze")
        image <- newMriImageFromAnalyze(fileNames)
    else if (fileNames$format == "Nifti")
        image <- newMriImageFromNifti(fileNames)
    
    invisible (image)
}

writeMriImageToFile <- function (image, fileName = NULL, fileType = NA, format = NA, singleFile = NA, gzipped = NA, datatype = NULL, overwrite = TRUE)
{
    if (!is.null(fileName))
        fileName <- expandFileName(fileName)
    else if (image$isInternal())
        output(OL$Error, "This image has no associated file name; it must be specified")
    else
        fileName <- image$getSource()
    
    params <- getParametersForFileType(fileType, errorIfInvalid=FALSE)
    if (is.null(params))
        params <- getParametersForFileType(getOption("tractorFileType"), errorIfInvalid=FALSE)
    if (is.null(params))
        params <- getParametersForFileType(NA, format, singleFile, gzipped, errorIfInvalid=TRUE)
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    
    if (overwrite)
        unlink(files[exist])
    else if (sum(exist) > 0)
        output(OL$Error, "File exists and cannot be overwritten")
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, params$headerSuffix)
    imageFile <- ensureFileSuffix(fileStem, params$imageSuffix)
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile)
    
    if (params$format == "Analyze")
        writeMriImageToAnalyze(image, fileNames, gzipped=params$gzipped, datatype=datatype)
    else if (params$format == "Nifti")
        writeMriImageToNifti(image, fileNames, gzipped=params$gzipped, datatype=datatype)
}
