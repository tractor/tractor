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
    fileSet <- ImageFileSet$new(auxiliaries=auxiliaries)
    format <- fileSet$findFormat(fileName, all=TRUE)
    
    if (is.null(format))
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
    else if (length(format$otherFiles) > 0L)
    {
        assert(!errorIfMissing, "Multiple compatible image files exist: #{fileName}")
        return (NULL)
    }
    
    oldFormatName <- ore_subst("^[a-z]", toupper, ore_subst("_.+$","",format$format))
    
    fileNames <- list(fileStem=format$stem, headerFile=format$headerFile, imageFile=format$imageFile, auxiliaryFiles=format$auxiliaryFiles, format=oldFormatName, headerSuffix=names(format$headerFile), imageSuffix=names(format$imageFile), auxiliarySuffixes=names(format$auxiliaryFiles))
    return (fileNames)
}

#' @rdname files
#' @export
imageFileExists <- function (fileName, fileType = NULL)
{
    return (imageFiles(fileName)$present())
}

#' @rdname files
#' @export
removeImageFiles <- function (fileName, ...)
{
    imageFiles(fileName)$delete()
}

#' @rdname files
#' @export
symlinkImageFiles <- function (from, to, overwrite = FALSE, relative = TRUE, ...)
{
    imageFiles(from)$symlink(to, overwrite=overwrite, relative=relative)
}

#' @rdname files
#' @export
copyImageFiles <- function (from, to, overwrite = FALSE, deleteOriginals = FALSE, ...)
{
    if (deleteOriginals)
        imageFiles(from)$move(to, overwrite=overwrite)
    else
        imageFiles(from)$copy(to, overwrite=overwrite)
}

chooseDataTypeForImage <- function (image, format, maxSize = NULL)
{
    if (image$isEmpty())
        return ("none")
    else if (image$isRgb())
        return (ifelse(image$nChannels() == 4L, "rgba", "rgb"))
    else if (image$isSparse())
        data <- image$getData()$getData()
    else
        data <- image$getData()
    
    # Get the available data types for the specified format
    datatypes <- .DatatypeCodes[[format]]
    
    # If double-mode data can be represented as integers, convert it to save space
    # Note that this slows the function down
    rType <- storage.mode(data)
    if (!is.null(maxSize) || (rType == "double" && equivalent(as.double(data),suppressWarnings(as.integer(data)))))
        rType <- "integer"
    
    if (rType == "double")
    {
        singleTypeExists <- "float" %in% names(datatypes)
        doubleTypeExists <- "double" %in% names(datatypes)
        assert(singleTypeExists || doubleTypeExists, "Floating-point data cannot be stored using the specified file format")
        
        if (singleTypeExists && (isTRUE(getOption("tractorOutputPrecision") == "single") || !doubleTypeExists))
            return ("float")
        else
            return ("double")
    }
    else if (rType == "complex")
    {
        singleTypeExists <- "cfloat" %in% names(datatypes)
        doubleTypeExists <- "cdouble" %in% names(datatypes)
        assert(singleTypeExists || doubleTypeExists, "Complex-valued data cannot be stored using the specified file format")
        
        if (singleTypeExists && (isTRUE(getOption("tractorOutputPrecision") == "single") || !doubleTypeExists))
            return ("cfloat")
        else
            return ("cdouble")
    }
    else
    {
        dataRange <- range(data, na.rm=TRUE)
        needSigned <- (dataRange[1] < 0)
        compatible <- names(datatypes) %~% ifelse(needSigned, "^int", "int")
        
        bitSizes <- as.numeric(ore_switch(names(datatypes), "\\d+$"="\\0", "0"))
        maximumValues <- 2^(bitSizes - as.integer(names(datatypes) %~% "^int")) - 1
        if (is.null(maxSize))
            compatible <- compatible & (max(abs(dataRange)) <= maximumValues)
        else
        {
            # If specified, the maximum datatype size must be obeyed. We then
            # fit the data if possible; otherwise we take the highest upper
            # bound available
            compatible <- compatible & (bitSizes <= maxSize * 8)
            compatible <- compatible & (max(abs(dataRange)) <= maximumValues | maximumValues == max(maximumValues[compatible]))
        }
        
        # Prefer Analyze-compatible data types for NIfTI files
        if (format == "Nifti" && any(datatypes[compatible] <= 64))
            compatible <- compatible & (datatypes <= 64)
        
        assert(any(compatible), "No compatible data type exists for the specified image and file format")
        
        maximumValues[!compatible] <- Inf
        return (names(datatypes)[which.min(maximumValues)])
    }
}

#' Working with MRI images stored in various formats
#' 
#' Functions for reading, writing, locating, copying and removing MRI images
#' stored in NIfTI, Analyze, MGH and MRtrix formats.
#' 
#' NIfTI and Analyze are related formats for storing magnetic resonance images.
#' NIfTI is a more recent extension of Analyze, and contains more specific
#' information about, for example, the orientation of the image. Its use is
#' therefore recommended where possible. MGH format is used by the popular
#' image processing package FreeSurfer, and MRtrix format by the software of
#' the same name. These formats use a number of different file extensions, but
#' the details are abstracted away from the user by these functions.
#' 
#' TractoR does not allow for files with the same basic name using multiple
#' Analyze/NIfTI/MGH/MRtrix formats in a single directory (e.g.
#' \code{"foo.nii"} AND \code{"foo.img"}), and these functions will produce an
#' error if multiple compatible files exist.
#' 
#' Suitable values for \code{fileType} (and the \code{tractorFileType} option,
#' which is used as a default for writing) are \code{"NIFTI"},
#' \code{"NIFTI_PAIR"} (the two-file NIfTI format), \code{"MGH"}, and
#' corresponding gzipped versions of these with \code{"_GZ"} appended. File
#' types \code{"ANALYZE"} and \code{"MRTRIX"}, and \code{"_GZ"} variants, are
#' additionally available for reading only. \code{"NIFTI_GZ"} is the default
#' value for the \code{tractorFileType} option, but that can be changed using a
#' call to \code{\link{options}}, or by setting the \code{TRACTOR_FILETYPE}
#' environment variable before loading the \code{tractor.base} package.
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
#' @param datatype A datatype string, such as \code{"uint8"} or \code{"float"},
#'   specifying the pixel datatype to use when storing the data. If specified,
#'   this must be a type supported by the requested (or default) file format.
#'   The default, \code{"fit"}, results in a datatype being chosen that is wide
#'   enough to fit the range of the data elements. An error will arise if
#'   there's no such type.
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
    pixdims <- pixdim(image)
    fullDims <- c(dims, rep(1,max(0,7-nDims)))
    nVoxels <- prod(dims)
    
    # Extract the datatype if the information is available
    if (!is.null(info$storage))
    {
        datatype <- info$storage$datatype
        report(OL$Debug, "Image datatype is #{datatype} (#{.Datatypes[[datatype]]$size}-byte #{ifelse(.Datatypes[[datatype]]$signed,'signed','unsigned')})")
    }
    report(OL$Debug, "Image orientation is #{orientation(image)}")
    
    # Check that the mask is compatible, if it was specified
    if (sparse && !is.null(mask) && !equivalent(dim(mask),dims[seq_along(dim(mask))]))
        report(OL$Error, "Mask and image dimensions do not match")
    
    # Check whether we need to read data and/or reorder the image
    data <- NULL
    willReadData <- (!metadataOnly && !RNifti:::hasData(image))
    willReorderImage <- (reorder && orientation(image) != "LAS")
    
    assert(!willReadData || datatype %in% names(.DatatypeCodes$Nifti), "Image datatype is not supported")
    
    inBlockOrder <- info$storage$blockOrder %||% TRUE
    swapEndianness <- info$storage$endian != "" && info$storage$endian != .Platform$endian
    
    # Data has not already been read, so do it here
    if (willReadData && inBlockOrder)
    {
        # Work out how many blocks (3D volumes) we're reading, and the corresponding byte offsets
        blockSize <- prod(fullDims[1:3])
        if (!is.null(volumes) && nDims > 3)
            blocks <- volumes
        else
            blocks <- seq_len(prod(fullDims[4:7]))
        offsets <- info$storage$offset + (blocks - 1) * blockSize * .Datatypes[[datatype]]$size
        
        coords <- values <- NULL
        for (i in seq_along(blocks))
        {
            # Read the current block
            currentData <- RNifti:::readBlob(fileNames$imageFile, blockSize, .DatatypeCodes$Nifti[datatype], offsets[i], swap=swapEndianness)
            
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
                    data <- array(as(0,.Datatypes[[datatype]]$rType), dim=c(fullDims[1:3],length(blocks)))
                data[,,,i] <- currentData
            }
        }
        
        # Create the sparse matrix from its components, if requested
        if (sparse)
            data <- newSparseArrayWithData(values, coords, dims)
        
        # Update the dimensions to match the image metadata
        dim(data) <- dims
    }
    else if (!inBlockOrder)
    {
        assert(length(dims) == 4, "Only 4D images out of block order are supported")
        
        perm <- c(2:4, 1L)
        image$dim[2:(nDims+1)] <- dims[perm]
        image$pixdim[2:(nDims+1)] <- pixdims[perm]
        
        if (willReadData)
        {
            data <- RNifti:::readBlob(fileNames$imageFile, nVoxels, .DatatypeCodes$Nifti[datatype], info$storage$offset, swap=swapEndianness)
            
            dim(data) <- dims
            data <- aperm(data, perm)
            
            if (!is.null(volumes))
                data <- data[,,,volumes]
            if (sparse)
                data <- as(data, "SparseArray")
        }
    }
    
    # Attach the data to the image
    if (willReadData)
        image <- updateNifti(data, image)
    
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
    
    # Read the auxiliary tags file, if one exists (TractoR or BIDS JSON)
    tagsFileName <- ensureFileSuffix(fileNames$fileStem, "tags")
    jsonFileName <- ensureFileSuffix(fileNames$fileStem, "json")
    if (file.exists(tagsFileName))
        do.call(image$setTags, yaml::yaml.load_file(tagsFileName))
    else if (file.exists(jsonFileName))
        do.call(image$setTags, fromBidsJson(jsonFileName, rename=TRUE))
    
    # Read diffusion directions, if present
    dirsFileNames <- ensureFileSuffix(fileNames$fileStem, c("dirs","bval","bvec"))
    if (file.exists(dirsFileNames[1]))
    {
        dirs <- as.matrix(read.table(dirsFileNames[1]))
        if (!is.null(volumes))
            dirs <- dirs[volumes,,drop=FALSE]
        if (ncol(dirs) == 4 && nrow(dirs) == image$nVolumes())
            image$setTags(bVectors=dirs[,1:3,drop=FALSE], bValues=dirs[,4])
        else
            flag(OL$Warning, "Auxiliary directions file does not match image - ignoring")
    }
    else if (all(file.exists(dirsFileNames[2:3])))
    {
        bValues <- drop(as.matrix(read.table(dirsFileNames[2])))
        directions <- t(as.matrix(read.table(dirsFileNames[3])))
        if (length(bValues) == image$nVolumes() && nrow(directions) == image$nVolumes())
            image$setTags(bVectors=directions, bValues=bValues)
        else
            flag(OL$Warning, "Auxiliary bval/bvec files do not match image - ignoring")
    }
    
    return (image)
}

writeImageData <- function (image, connection, datatype, endian = .Platform$endian)
{
    # RNifti handles RGB(A) for NIfTI/ANALYZE; no other write format supports it
    assert(!(datatype %in% c("rgb","rgba")), "Cannot write RGB data at the moment")
    
    image <- as(image, "MriImage")
    data <- image$getData()
    dims <- image$getDimensions()
    
    # writeBin() can only write 2^31 - 1 bytes, so work blockwise if necessary
    if (image$isSparse() || prod(dims) * .Datatypes[[datatype]]$size >= 2^31)
    {
        nDims <- image$getDimensionality()
        for (i in seq_len(dims[nDims]))
        {
            indices <- alist(i=,j=,k=,t=,u=,v=,w=)[1:nDims]
            indices[[nDims]] <- i
            currentData <- as.array(do.call("[", c(list(data),indices)))
            
            storage.mode(currentData) <- .Datatypes[[datatype]]$rType
            attributes(currentData) <- NULL
            writeBin(currentData, connection, size=.Datatypes[[datatype]]$size, endian=endian)
        }
    }
    else
    {
        storage.mode(data) <- .Datatypes[[datatype]]$rType
        attributes(data) <- NULL
        writeBin(data, connection, size=.Datatypes[[datatype]]$size, endian=endian)
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
writeImageFile <- function (image, fileName = NULL, fileType = NA, overwrite = TRUE, datatype = "fit", writeTags = FALSE)
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
        writeNifti(image, fileNames, datatype=datatype)
    else if (params$format == "Mgh")
        writeMgh(image, fileNames, datatype=datatype, gzipped=params$gzipped)
    
    if (image$isInternal())
    {
        image$setSource(expandFileName(fileStem))
        if (Sys.getenv("TRACTOR_COMMANDLINE") != "")
            image$setTags(commandHistory=Sys.getenv("TRACTOR_COMMANDLINE"), merge=TRUE)
    }
    
    if ((writeTags || file.exists(ensureFileSuffix(fileStem,"tags"))) && image$nTags() > 0)
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
