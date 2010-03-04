.MriImageMetadata <- function (.imagedims, .voxdims, .voxunit, .source, .datatype, .origin, .endian)
{
    if (!is.list(.datatype) || length(unlist(.datatype[c("type","size","isSigned")])) != 3)
    {
        flag(OL$Warning, "Specified image data type is not valid - ignoring it")
        .datatype <- NULL
    }
    
    self <- list(
        getDataType = function () { return (.datatype) },
        
        getDimensionality = function () { return (length(.voxdims)) },
        
        getDimensions = function () { return (.imagedims) },
        
        getEndianness = function () { return (.endian) },
        
        getFieldOfView = function () { return (abs(.voxdims) * .imagedims) },
        
        getOrigin = function () { return (.origin) },
        
        getSource = function () { return (.source) },
        
        getVoxelDimensions = function () { return (.voxdims) },
        
        getVoxelUnit = function () { return (.voxunit) },
        
        isInternal = function () { return (.source == "internal") },
        
        setEndianness = function (newEndian)
        {
            if (newEndian %in% c("big","little"))
                .endian <<- newEndian
        },
        
        setSource = function (newSource)
        {
            if (is.character(newSource) && (length(newSource) == 1))
                .source <<- newSource
        },
        
        summarise = function ()
        {
            if (is.null(.datatype))
                datatypeString <- "undefined"
            else
            {
                datatypeString <- ifelse(.datatype$isSigned, "signed", "unsigned")
                datatypeString <- paste(datatypeString, " ", .datatype$type, ", ", .datatype$size*8, " bits/voxel", sep="")
            }
            
            output(OL$Info, "Image source     : ", .source)
            output(OL$Info, "Image dimensions : ", implode(.imagedims, sep=" x "), " voxels")
            output(OL$Info, "Coordinate origin: (", implode(round(.origin,2), sep=","), ")")
            output(OL$Info, "Voxel dimensions : ", implode(round(abs(.voxdims),5), sep=" x "), " ", .voxunit)
            output(OL$Info, "Data type        : ", datatypeString)
            output(OL$Info, "Endianness       : ", self$getEndianness())
        },
        
        summarize = function () { self$summarise() }
    )
    
    class(self) <- c("metadata.image.mri", "list.object", "list")
    invisible (self)
}

.MriImage <- function (.data, .metadata)
{
    if (!isMriImageMetadata(.metadata))
        output(OL$Error, "Metadata object is invalid")
    if (!is.array(.data))
        output(OL$Error, "Image data is not an array")
    if (!equivalent(dim(.data), .metadata$getDimensions()))
        output(OL$Error, "Data dimensions do not match those stored in the metadata object")
    
    self <- list(
        getData = function () { return (.data) },
        
        getDataAtPoint = function (...)
        {
            dim <- self$getDimensionality()
            loc <- resolveVector(len=dim, ...)
            if (is.null(loc) || (length(...) != dim))
                output(OL$Error, "Point must be specified as a ", dim, "-vector")
                
            if (all(loc >= 1) && all(loc <= self$getDimensions()))
                return (.data[matrix(loc,nrow=1)])
            else
                return (NA)
        },
        
        getMetadata = function () { return (.metadata) }
    )
    
    class(self) <- c("image.mri", "list.object", "list")
    self <- inherit(self, .metadata)
    invisible (self)
}

isMriImageMetadata <- function (object)
{
    return ("metadata.image.mri" %in% class(object))
}

isMriImage <- function (object)
{
    return ("image.mri" %in% class(object))
}

deserialiseMriImageMetadata <- function (file = NULL, object = NULL)
{
    metadata <- deserialiseListObject(file, object, .MriImageMetadata)
    invisible (metadata)
}

deserialiseMriImage <- function (file = NULL, object = NULL)
{
    if (is.null(object))
        object <- deserialiseListObject(file, raw=TRUE)
    
    if (isDeserialisable(object$metadata, "metadata.image.mri"))
        object$metadata <- deserialiseMriImageMetadata(object=object$metadata)
    else
        output(OL$Error, "Deserialised object contains no valid metadata")
    
    image <- deserialiseListObject(NULL, object, .MriImage)
    invisible (image)
}

"[.image.mri" <- function (x, ...)
{
    return (x$getData()[...])
}

"[<-.image.mri" <- function (x, ..., value)
{
    data <- x$getData()
    data[...] <- value
    newImage <- .MriImage(data, x$getMetadata())
    newImage$setSource("internal")
    return (newImage)
}

Math.image.mri <- function (x, ...)
{
    newImage <- newMriImageWithSimpleFunction(x, .Generic, ...)
    return (newImage)
}

Ops.image.mri <- function (e1, e2)
{
    newImage <- newMriImageWithBinaryFunction(e1, e2, .Generic)
    return (newImage)
}

Summary.image.mri <- function (..., na.rm = FALSE)
{
    if (nargs() > 2)
        output(OL$Error, "Function ", .Generic, " is not defined for more than one image object")
    
    result <- get(.Generic)((...)$getData())
    return (result)
}

newMriImageMetadataFromTemplate <- function (metadata, imageDims = NA, voxelDims = NA, voxelUnit = NA, source = "internal", datatype = NA, origin = NA, endian = NA)
{
    if (!isMriImageMetadata(metadata))
        output(OL$Error, "The specified metadata template is not valid")
    
    template <- serialiseListObject(metadata)
    params <- list(imagedims=imageDims,
                   voxdims=voxelDims,
                   voxunit=voxelUnit,
                   source=source,
                   datatype=datatype,
                   origin=origin,
                   endian=endian)
    params <- params[!is.na(params)]
    
    composite <- c(params, template)
    composite <- composite[!duplicated(names(composite))]
    
    newMetadata <- .MriImageMetadata(composite$imagedims, composite$voxdims, composite$voxunit, composite$source, composite$datatype, composite$origin, composite$endian)
    invisible (newMetadata)
}

newMriImageWithData <- function (data, metadata)
{
    # Do not use the metadata object supplied because it is mutable and we
    # don't want changes (to e.g. endianness) to affect multiple images
    metadataDuplicate <- newMriImageMetadataFromTemplate(metadata)
    image <- .MriImage(data, metadataDuplicate)
    invisible (image)
}

newMriImageFromTemplate <- function (image, ...)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    oldMetadata <- image$getMetadata()
    newMetadata <- newMriImageMetadataFromTemplate(oldMetadata, ...)
    
    if (equivalent(oldMetadata$getDimensions(), newMetadata$getDimensions()))
        newImage <- .MriImage(image$getData(), newMetadata)
    else
    {
        if (prod(oldMetadata$getDimensions()) != prod(newMetadata$getDimensions()))
            flag(OL$Warning, "The requested image dimensions will not result in a perfect reshaping")
        newData <- array(as.vector(image$getData()), dim=newMetadata$getDimensions())
        newImage <- .MriImage(newData, newMetadata)
    }
    
    invisible (newImage)
}

newMriImageWithSimpleFunction <- function (image, fun, ..., newDataType = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    fun <- match.fun(fun)
    newData <- fun(image$getData(), ...)
    newDataType <- (if (is.null(newDataType)) image$getDataType() else newDataType)
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata(), datatype=newDataType)
    
    image <- .MriImage(newData, metadata)
    invisible (image)
}

newMriImageWithBinaryFunction <- function (image1, image2, fun, ..., newDataType = NULL)
{
    if (!isMriImage(image1) || !isMriImage(image2))
        output(OL$Error, "The specified images are not MriImage objects")
    
    fun <- match.fun(fun)
    newData <- fun(image1$getData(), image2$getData(), ...)
    newDataType <- (if (is.null(newDataType)) image1$getDataType() else newDataType)
    metadata <- newMriImageMetadataFromTemplate(image1$getMetadata(), datatype=newDataType)
    
    image <- .MriImage(newData, metadata)
    invisible (image)
}

extractDataFromMriImage <- function (image, dim, loc)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() < max(2,dim))
        output(OL$Error, "The dimensionality of the specified image is too low")
    if (!(loc %in% 1:(image$getDimensions()[dim])))
        output(OL$Error, "The specified location is out of bounds")
    
    data <- image$getData()
    dimsToKeep <- setdiff(1:image$getDimensionality(), dim)
    
    # This "apply" call is a cheeky bit of R wizardry (credit: Peter Dalgaard)
    newData <- apply(data, dimsToKeep, "[", loc)
    if (is.vector(newData))
        newData <- promote(newData)
    
    invisible (newData)
}

newMriImageByExtraction <- function (image, dim, loc)
{
    newData <- extractDataFromMriImage(image, dim, loc)
    
    dimsToKeep <- setdiff(1:image$getDimensionality(), dim)
    metadata <- serialiseListObject(image$getMetadata())
    newMetadata <- .MriImageMetadata(metadata$imagedims[dimsToKeep], metadata$voxdims[dimsToKeep], metadata$voxunit, "internal", metadata$datatype, metadata$origin[dimsToKeep], metadata$endian)
        
    image <- .MriImage(newData, newMetadata)
    invisible (image)
}

newMriImageByMasking <- function (image, mask)
{
    if (!identical(image$getDimensions(), dim(mask)))
        output(OL$Error, "The specified image and mask do not have the same dimensions")
    if (!is.logical(mask))
        output(OL$Error, "Mask must be specified as an array of logical values")
    
    newData <- image$getData() * mask
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata())
    
    image <- .MriImage(newData, metadata)
    invisible (image)
}

newMriImageByThresholding <- function (image, level, defaultValue = 0)
{
    thresholdFunction <- function (x) { return (ifelse(x >= level, x, defaultValue)) }
    newImage <- newMriImageWithSimpleFunction(image, thresholdFunction)
    invisible (newImage)
}

newMriImageByTrimming <- function (image, clearance = 4)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    if (length(clearance) == 1)
        clearance <- rep(clearance, image$getDimensionality())
    
    data <- image$getData()
    dims <- image$getDimensions()
    indices <- lapply(seq_len(image$getDimensionality()), function (i) {
        dimMax <- apply(data, i, max)
        toKeep <- which(is.finite(dimMax) & dimMax > 0)
        if (length(toKeep) == 0)
            output(OL$Error, "Trimming the image would remove its entire contents")
        minLoc <- max(1, min(toKeep)-clearance[i])
        maxLoc <- min(dims[i], max(toKeep)+clearance[i])
        return (minLoc:maxLoc)
    })
    
    data <- do.call("[", c(list(data),indices,list(drop=FALSE)))
    newDims <- sapply(indices, length)
    
    # NB: Origin is not corrected here
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata(), imageDims=newDims)
    newImage <- newMriImageWithData(data, metadata)
    
    invisible (newImage)
}
