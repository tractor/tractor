getParametersForFileType <- function (fileType = NA, format = NA, singleFile = NA, gzipped = NA, errorIfInvalid = TRUE)
{
    if (is.character(fileType))
        typeIndex <- which(.FileTypes$typeNames == toupper(fileType))
    else
        typeIndex <- which(.FileTypes$formatNames == format & .FileTypes$singleFile == singleFile & .FileTypes$gzipped == gzipped)
    
    if (length(typeIndex) != 1)
    {
        if (errorIfInvalid)
            report(OL$Error, "Specified file type information is incomplete or invalid")
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

#' @rdname files
#' @export
identifyImageFileNames <- function (fileName, fileType = NULL, errorIfMissing = TRUE, auxiliaries = c("dirs","lut","tags"), ...)
{
    suffixes <- unique(c(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes, auxiliaries))
    fileName <- expandFileName(fileName)
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    headersExist <- intersect(unique(.FileTypes$headerSuffixes), suffixes[exist])
    imagesExist <- intersect(unique(.FileTypes$imageSuffixes), suffixes[exist])
    auxiliariesExist <- intersect(unique(auxiliaries), suffixes[exist])
    
    if (length(headersExist) < 1 || length(imagesExist) < 1)
    {
        originalFileName <- fileName
        fileName <- resolvePath(originalFileName, ...)
        
        # The image does not exist, because we have already tried the unresolved path
        if (all(fileName == originalFileName))
        {
            if (errorIfMissing)
                report(OL$Error, "Complete image file does not exist: #{fileName}")
            else
                return (NULL)
        }
        else
            return (identifyImageFileNames(fileName, fileType=fileType, errorIfMissing=errorIfMissing, auxiliaries=auxiliaries))
    }
    else if (length(headersExist) > 1 || length(imagesExist) > 1)
    {
        if (errorIfMissing)
            report(OL$Error, "Multiple compatible image files exist: #{fileName}")
        else
            return (NULL)
    }
    
    typeIndices <- which(.FileTypes$headerSuffixes == headersExist &
                         .FileTypes$imageSuffixes == imagesExist)
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, headersExist)
    imageFile <- ensureFileSuffix(fileStem, imagesExist)
    auxiliaryFiles <- ensureFileSuffix(fileStem, auxiliariesExist)
    
    # ANALYZE and NIFTI_PAIR file types use the same filename suffixes
    if (length(typeIndices) == 1)
        format <- .FileTypes$format[typeIndices]
    else if (!is.null(fileType))
        format <- (getParametersForFileType(fileType, errorIfInvalid=TRUE))$format
    else
        format <- ifelse(niftiVersion(headerFile) == 0, "Analyze", "Nifti")
    
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile, auxiliaryFiles=auxiliaryFiles, format=format, headerSuffix=headersExist, imageSuffix=imagesExist, auxiliarySuffixes=auxiliariesExist)
    return (fileNames)
}

#' @rdname files
#' @export
imageFileExists <- function (fileName, fileType = NULL)
{
    return (sapply(fileName, function (file) {
        !is.null(identifyImageFileNames(file, fileType, errorIfMissing=FALSE))
    }))
}

#' @rdname files
#' @export
removeImageFiles <- function (fileName, ...)
{
    info <- identifyImageFileNames(fileName, ...)
    if (!is.null(info))
    {
        files <- unique(c(info$headerFile, info$imageFile, info$auxiliaryFiles))
        report(OL$Debug, "Unlinking files #{embrace(files)}")
        unlink(files)
    }
}

#' @rdname files
#' @export
symlinkImageFiles <- function (from, to, overwrite = FALSE, relative = TRUE, ...)
{
    if (isTRUE(getOption("tractorNoSymlinks")))
        return (copyImageFiles(from, to, overwrite, ...))
    
    if (length(from) != length(to))
        report(OL$Error, "The number of source and target file names must match")
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    for (i in seq_along(from))
    {
        info <- identifyImageFileNames(from[i], ...)
        currentSource <- unique(c(info$headerFile, info$imageFile, info$auxiliaryFiles))
        currentTarget <- unique(ensureFileSuffix(expandFileName(to[i]), c(info$headerSuffix,info$imageSuffix,info$auxiliarySuffixes), strip=suffixes))
        
        # NB: file.exists() requires the target of an existing link to exist, but we want to know whether the link itself exists
        if (overwrite && any(!is.na(Sys.readlink(currentTarget))))
            unlink(currentTarget)
        if (relative)
        {
            for (j in seq_along(currentSource))
                currentSource[j] <- relativePath(currentSource[j], currentTarget[j])
        }
        
        report(OL$Verbose, "Linking #{embrace(currentSource)} => #{embrace(currentTarget)}")
        file.symlink(currentSource, currentTarget)
    }
}

#' @rdname files
#' @export
copyImageFiles <- function (from, to, overwrite = FALSE, deleteOriginals = FALSE, ...)
{
    if (length(from) != length(to))
        report(OL$Error, "The number of source and target file names must match")
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    for (i in seq_along(from))
    {
        info <- identifyImageFileNames(from[i], ...)
        currentSource <- c(info$headerFile, info$imageFile, info$auxiliaryFiles)
        currentTarget <- ensureFileSuffix(expandFileName(to[i]), c(info$headerSuffix,info$imageSuffix,info$auxiliarySuffixes), strip=suffixes)
        
        # Don't try to copy an image onto itself
        if (all(currentSource == currentTarget))
            next
        
        report(OL$Verbose, "#{ifelse(deleteOriginals,'Moving','Copying')} #{embrace(unique(currentSource))} => #{embrace(unique(currentTarget))}")
        success <- file.copy(unique(currentSource), unique(currentTarget), overwrite=overwrite)
        
        if (all(success) && deleteOriginals)
            removeImageFiles(from[i], ...)
    }
}

chooseDataTypeForImage <- function (image, format)
{
    if (image$isEmpty())
        return (NULL)
    else if (image$isSparse())
        data <- image$getData()$getData()
    else
        data <- image$getData()
    
    # Get the available data types for the specified format
    datatypes <- get(paste(".",format,sep=""))$datatypes
    
    # If double-mode data can be represented as integers, convert it to save space
    # Note that this slows the function down
    rType <- storage.mode(data)
    if (rType == "double" && equivalent(as.double(data),suppressWarnings(as.integer(data))))
        rType <- "integer"
    
    isSigned <- (rType == "double" || min(data,na.rm=TRUE) < 0)
    
    if (rType == "double")
    {
        singleTypeExists <- sum(datatypes$rTypes == "double" & datatypes$sizes == 4) == 1
        doubleTypeExists <- sum(datatypes$rTypes == "double" & datatypes$sizes == 8) == 1
        assert(singleTypeExists || doubleTypeExists, "Floating-point data cannot be stored using the specified file format")
        
        if (singleTypeExists && (isTRUE(getOption("tractorOutputPrecision") == "single") || !doubleTypeExists))
            size <- 4
        else
            size <- 8
        
        isSigned <- TRUE
        code <- datatypes$codes[datatypes$rTypes == "double" & datatypes$sizes == size]
        name <- datatypes$names[datatypes$codes == code]
    }
    else
    {
        compatible <- (datatypes$rTypes == "integer")
        if (min(data,na.rm=TRUE) < 0)
            compatible <- compatible & datatypes$isSigned
        
        maximumValues <- 2^(datatypes$sizes*8 - as.integer(datatypes$isSigned)) - 1
        largestAbsoluteDataValue <- max(abs(max(data,na.rm=TRUE)), abs(min(data,na.rm=TRUE)))
        compatible <- compatible & (largestAbsoluteDataValue <= maximumValues)
        
        # Prefer Analyze-compatible data types for NIfTI files
        if (format == "Nifti" && any(compatible[datatypes$codes <= 64]))
            compatible <- compatible & (datatypes$codes <= 64)
        
        assert(any(compatible), "No compatible data type exists for the specified image and file format")
        
        maximumValues[!compatible] <- Inf
        code <- datatypes$codes[which.min(maximumValues)]
        size <- datatypes$sizes[datatypes$codes == code]
        isSigned <- datatypes$isSigned[datatypes$codes == code]
        name <- datatypes$names[datatypes$codes == code]
    }
    
    return (list(code=code, type=rType, size=size, isSigned=isSigned, name=name))
}

#' Working with MRI images stored in NIfTI, Analyze and MGH formats
#' 
#' Functions for reading, writing, locating, copying and removing MRI images
#' stored in NIfTI, Analyze and MGH formats.
#' 
#' NIfTI and Analyze are related formats for storing magnetic resonance images.
#' NIfTI is a more recent extension of Analyze, and contains more specific
#' information about, for example, the orientation of the image. Its use is
#' therefore recommended where possible. MGH format is used by the popular
#' image processing package FreeSurfer. These formats use a number of different
#' file extensions, but the details are abstracted away from the user by these
#' functions.
#' 
#' TractoR does not allow for files with the same basic name using multiple
#' Analyze/NIfTI/MGH formats in a single directory (e.g. \code{"foo.nii"} AND
#' \code{"foo.img"}), and these functions will produce an error if multiple
#' compatible files exist.
#' 
#' Suitable values for \code{fileType} (and the \code{tractorFileType} option,
#' which is used as a default) are \code{ANALYZE}, \code{NIFTI},
#' \code{NIFTI_PAIR} (the two-file NIfTI format), \code{MGH},
#' \code{ANALYZE_GZ}, \code{NIFTI_GZ}, \code{NIFTI_PAIR_GZ} and \code{MGH_GZ}.
#' The latter four are gzipped versions of the former four. \code{NIFTI_GZ} is
#' recommended unless there is a need for one of the others. This is the
#' default value for the \code{tractorFileType} option, but that can be changed
#' using a call to \code{\link{options}}, or by setting the
#' \code{TRACTOR_FILETYPE} environment variable before loading the tractor.base
#' package.
#' 
#' Since multiple files may be involved, copying, moving or symlinking images
#' is not trivial. \code{copyImageFiles} and \code{symlinkImageFiles} are
#' wrappers around the standard functions \code{\link{file.copy}} and
#' \code{\link{file.symlink}} which handle this complexity.
#' 
#' @param fileName,from,to File names, with or without appropriate extension.
#' @param image An \code{\linkS4class{MriImage}} object.
#' @param fileType A character vector of length one, giving the file type
#'   required or expected. If this option is missing, the file type used for
#'   writing images will be taken from the \code{tractorFileType} option. See
#'   Details.
#' @param auxiliaries A character vector of auxiliary file suffixes to search
#'   for.
#' @param metadataOnly Logical value: if \code{TRUE}, only metadata are read
#'   into the object.
#' @param volumes An optional integer vector specifying a subset of volumes to
#'   read (generally to save memory). If given, only the requested volumes in
#'   the 4D file will be read.
#' @param sparse Logical value: should the image data be stored in a
#'  \code{\linkS4class{SparseArray}} object?
#' @param mask An optional \code{\linkS4class{MriImage}} object representing a
#'   mask, outside of which the image to be read should be considered to be
#'   zero. This can be used to save memory when only a small part of a large
#'   image is of interest. Ignored if \code{sparse} is not \code{TRUE}.
#' @param reorder Logical value: should the image data be reordered to LAS?
#'   This is recommended in most circumstances.
#' @param \dots For \code{identifyImageFileNames}, additional arguments to
#'   \code{\link{resolvePath}}. Elsewhere, additional arguments to
#'   \code{identifyImageFileNames}.
#' @param overwrite Logical value: overwrite an existing image file? For
#'   \code{writeImageFile}, an error will be raised if there is an existing
#'   file and this is set to FALSE.
#' @param maxSize If not \code{NULL}, the maximum number of bytes per pixel to
#'   use when storing the data. This can lead to a substantial loss of
#'   precision, and is usually not desirable. Only used when writing to the
#'   NIfTI file format.
#' @param writeTags Logical value: should tags be written in YAML format to an
#'   auxiliary file?
#' @param errorIfMissing Logical value: raise an error if no suitable files
#'   were found?
#' @param deleteOriginals Logical value: if \code{TRUE}, \code{copyImageFiles}
#'   performs a move rather than a copy.
#' @param relative Logical value: if \code{TRUE}, the path stored in the
#'   symlink will be relative (e.g. \code{"../some_dir/some_image.nii"}) rather
#'   than absolute (e.g. \code{"/path/to/some_dir/some_image.nii"}).
#' @return \code{readImageFile} returns an \code{\linkS4class{MriImage}}
#'   object. \code{imageFileExists} returns \code{TRUE} if an existing file
#'   with the specified name exists (all file extensions are checked), and
#'   \code{FALSE} otherwise. \code{removeImageFiles} returns the result of
#'   \code{\link{unlink}} applied to all relevant files. \code{writeImageFile}
#'   and \code{identifyImageFileNames} return a list with the following elements,
#'   describing the identified or written files:
#'   \describe{
#'     \item{fileStem}{The file name without extension.}
#'     \item{headerFile}{The full header file name.}
#'     \item{imageFile}{The full image file name.}
#'     \item{format}{The format of the files (\code{"Nifti"}, \code{"Analyze"}
#'       or \code{"Mgh"}). Not returned by \code{writeImageFile}.}
#'   }
#'   \code{copyImageFiles} and \code{symlinkImageFiles} are called for their
#'   side effects.
#' 
#' @author Jon Clayden
#' @seealso The NIfTI-1 standard (\url{http://nifti.nimh.nih.gov/nifti-1}) and
#'   \code{\linkS4class{MriImage}}.
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @rdname files
#' @export
readImageFile <- function (fileName, fileType = NULL, metadataOnly = FALSE, volumes = NULL, sparse = FALSE, mask = NULL, reorder = TRUE, ...)
{
    # Find the relevant files
    fileNames <- identifyImageFileNames(fileName, fileType, ...)
    
    # Parse the files
    # These functions should return either a complete image or a NIfTI-like
    # header and information on the type and location of the data
    if (fileNames$format %in% c("Analyze","Nifti"))
        info <- readNifti(fileNames, metadataOnly=metadataOnly, volumes=volumes)
    else if (fileNames$format == "Mgh")
        info <- readMgh(fileNames)
    else if (fileNames$format == "Mrtrix")
        info <- readMrtrix(fileNames)
    else
        report(OL$Error, "Unknown image file format: #{fileNames$format}")
    
    # Create a niftiImage object from the image and/or header information
    if (!is.null(info$image))
        image <- updateNifti(info$image, info$header)
    else if (!is.null(info$header))
        image <- retrieveNifti(info$header)
    else
        report(OL$Error, "No image information is available")
    
    # Extract some metadata
    nDims <- ndim(image)
    dims <- dim(image)
    fullDims <- c(dims, rep(1,max(0,7-nDims)))
    nVoxels <- prod(dims)
    
    # Extract the datatype if the information is available
    if (!is.null(info$storage))
    {
        datatype <- info$storage$datatype
        report(OL$Debug, "Image datatype is #{datatype$size}-byte #{ifelse(datatype$isSigned,'signed','unsigned')} #{datatype$type}")
    }
    report(OL$Debug, "Image orientation is #{orientation(image)}")
    
    # Check that the mask is compatible, if it was specified
    if (sparse && !is.null(mask) && !equivalent(dim(mask),dims[seq_along(dim(mask))]))
        report(OL$Error, "Mask and image dimensions do not match")
    
    # Check whether we need to read data and/or reorder the image
    data <- NULL
    willReadData <- (!metadataOnly && !RNifti:::hasData(image))
    willReorderImage <- (reorder && orientation(image) != "LAS")
    
    # Data has not already been read, so do it here
    if (willReadData)
    {
        matchingDatatypes <- .Nifti$datatypes$rTypes == datatype$type & .Nifti$datatypes$sizes == datatype$size & .Nifti$datatypes$isSigned == datatype$isSigned
        assert(any(matchingDatatypes), "Image datatype is not supported")
        datatypeCode <- as.integer(.Nifti$datatypes$codes[which(matchingDatatypes)[1]])
        
        swapEndianness <- info$storage$endian != "" && info$storage$endian != .Platform$endian
        
        # Work out how many blocks (3D volumes) we're reading, and the corresponding byte offsets
        blockSize <- prod(fullDims[1:3])
        if (!is.null(volumes) && nDims > 3)
            blocks <- volumes
        else
            blocks <- seq_len(prod(fullDims[4:7]))
        offsets <- info$storage$offset + (blocks - 1) * blockSize * datatype$size
        
        coords <- values <- NULL
        for (i in seq_along(blocks))
        {
            # Read the current block
            currentData <- RNifti:::readBlob(fileNames$imageFile, blockSize, datatypeCode, offsets[i], swap=swapEndianness)
            
            if (sparse)
            {
                # A sparse result was requested, so identify nonzero and/or within-mask voxels
                toKeep <- which(currentData != 0)
                if (!is.null(mask))
                    toKeep <- intersect(toKeep, mask$find(array=FALSE))
                
                if (length(toKeep) > 0)
                {
                    coords <- rbind(coords, cbind(vectorToMatrixLocs(toKeep,fullDims[1:3]),blocks[i]))
                    values <- c(values, currentData[toKeep])
                }
            }
            else if (length(blocks) == 1)
                data <- currentData
            else
            {
                # Create an array for the data if it doesn't yet exist, then insert the block
                if (is.null(data))
                    data <- array(as(0,datatype$type), dim=c(fullDims[1:3],length(blocks)))
                data[,,,i] <- currentData
            }
        }
        
        # Create the sparse matrix from its components, if requested
        if (sparse)
            data <- newSparseArrayWithData(values, coords, dims)
        
        # Update the dimensions to match the image metadata
        dim(data) <- dims
        
        # Attach the data to the image
        image <- updateNifti(data, image)
    }
    
    # Reorder the image if requested (as opposed to the data to be associated with it)
    if (willReorderImage)
        orientation(image) <- "LAS"
    
    # Convert to an MriImage
    attr(image, "reordered") <- reorder
    image <- as(image, "MriImage")
    
    # Ensure the data has the required form
    if (metadataOnly)
        image$setData(NULL)
    else if (sparse && !image$isSparse())
        image$setData(as(image$getData(), "SparseArray"))
    
    # Set the source
    image$setSource(fileNames$fileStem)
    
    # If the reader produced any tags, set them first (auxiliary files will overwrite duplicates)
    if (!is.null(info$tags))
        do.call(image$setTags, as.list(info$tags))
    
    # Read the auxiliary tags file, if one exists
    tagsFileName <- ensureFileSuffix(fileNames$fileStem, "tags")
    if (file.exists(tagsFileName))
        do.call(image$setTags, yaml::yaml.load_file(tagsFileName))
    
    # Read diffusion directions, if present
    dirsFileName <- ensureFileSuffix(fileNames$fileStem, "dirs")
    if (file.exists(dirsFileName))
    {
        dirs <- as.matrix(read.table(dirsFileName))
        if (!is.null(volumes))
            dirs <- dirs[volumes,,drop=FALSE]
        if (ncol(dirs) == 4 && nrow(dirs) == image$nVolumes())
            image$setTags(bVectors=dirs[,1:3,drop=FALSE], bValues=dirs[,4])
        else
            flag(OL$Warning, "Auxiliary directions file does not match image - ignoring")
    }
    
    return (image)
}

writeImageData <- function (image, connection, type, size, endian = .Platform$endian)
{
    image <- as(image, "MriImage")
    data <- image$getData()
    dims <- image$getDimensions()
    
    # writeBin() can only write 2^31 - 1 bytes, so work blockwise if necessary
    if (image$isSparse() || prod(dims) * size >= 2^31)
    {
        nDims <- image$getDimensionality()
        for (i in seq_len(dims[nDims]))
        {
            indices <- alist(i=,j=,k=,t=,u=,v=,w=)[1:nDims]
            indices[[nDims]] <- i
            currentData <- as.array(do.call("[", c(list(data),indices)))
            
            storage.mode(currentData) <- type
            attributes(currentData) <- NULL
            writeBin(currentData, connection, size=size, endian=endian)
        }
    }
    else
    {
        storage.mode(data) <- type
        attributes(data) <- NULL
        writeBin(data, connection, size=size, endian=endian)
    }
}

writeGradientDirections <- function (directions, bValues, fileName)
{
    directions[is.na(directions)] <- 0
    bValues[is.na(bValues)] <- 0
    directions <- cbind(promote(directions,byrow=TRUE), bValues)
    lines <- ore.subst("\\.0+\\s*$", "", apply(format(directions,scientific=FALSE),1,implode,sep="  "))
    writeLines(lines, fileName)
}

#' @rdname files
#' @export
writeImageFile <- function (image, fileName = NULL, fileType = NA, overwrite = TRUE, maxSize = NULL, writeTags = FALSE)
{
    image <- as(image, "MriImage")
    
    if (!is.null(fileName))
        fileName <- expandFileName(fileName)
    else if (image$isInternal())
        report(OL$Error, "This image has no associated file name; it must be specified")
    else
        fileName <- image$getSource()
    
    params <- getParametersForFileType(fileType, errorIfInvalid=FALSE)
    if (is.null(params))
        params <- getParametersForFileType(getOption("tractorFileType"), errorIfInvalid=FALSE)
    
    suffixes <- union(.FileTypes$headerSuffixes, .FileTypes$imageSuffixes)
    
    files <- ensureFileSuffix(fileName, suffixes)
    exist <- file.exists(files)
    
    if (overwrite)
        unlink(files[exist])
    else if (sum(exist) > 0)
        report(OL$Error, "File exists and cannot be overwritten")
    
    fileStem <- ensureFileSuffix(fileName, NULL, strip=suffixes)
    headerFile <- ensureFileSuffix(fileStem, params$headerSuffix)
    imageFile <- ensureFileSuffix(fileStem, params$imageSuffix)
    fileNames <- list(fileStem=fileStem, headerFile=headerFile, imageFile=imageFile)
    
    if (!image$isReordered() && params$format != "Nifti")
        report(OL$Error, "An unreordered image can only be written to NIfTI format")
    
    if (params$format == "Analyze")
        report(OL$Error, "Writing to ANALYZE format is no longer supported")
    else if (params$format == "Nifti")
        writeNifti(image, fileNames, maxSize=maxSize)
    else if (params$format == "Mgh")
        writeMgh(image, fileNames, gzipped=params$gzipped)
    
    if (image$isInternal())
    {
        image$setSource(expandFileName(fileStem))
        if (Sys.getenv("TRACTOR_COMMANDLINE") != "")
            image$setTags(commandHistory=Sys.getenv("TRACTOR_COMMANDLINE"), merge=TRUE)
    }
    
    if (writeTags && image$nTags() > 0)
    {
        tags <- image$getTags()
        if (all(c("bVectors","bValues") %in% names(tags)))
            writeGradientDirections(image$getTags("bVectors"), image$getTags("bValues"), ensureFileSuffix(fileStem,"dirs"))
        tags <- tags[!(names(tags) %in% c("bVectors","bValues"))]
        if (length(tags) > 0)
            writeLines(yaml::as.yaml(tags,handlers=list(Date=format.Date)), ensureFileSuffix(fileStem,"tags"))
    }
    
    invisible (fileNames)
}
