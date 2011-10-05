MriImageMetadata <- setRefClass("MriImageMetadata", contains="SerialisableObject", fields=list(imagedims="integer",voxdims="numeric",voxunit="character",source="character",datatype="list",origin="numeric",storedXform="matrix",tags="list"), methods=list(
    initialize = function (imagedims = NULL, voxdims = NULL, voxunit = NULL, source = "internal", datatype = NULL, origin = NULL, storedXform = NA, tags = list(), ...)
    {
        if (length(tags) != 0 && !all(c("keys","values") %in% names(tags)))
            report(OL$Error, "Tag list must be empty, or else contain \"keys\" and \"values\" components")
        if (is.null(voxunit))
            voxunit <- "unknown"
        
        object <- initFields(imagedims=as.integer(imagedims), voxdims=as.numeric(voxdims), voxunit=voxunit, source=source, datatype=as.list(datatype), origin=as.numeric(origin), storedXform=as.matrix(storedXform), tags=tags)
        
        if (!is.null(datatype) && !all(c("type","size","isSigned") %in% names(object$datatype)))
        {
            flag(OL$Warning, "Specified image data type is not valid - ignoring it")
            object$datatype <- list()
        }
        
        return (object)
    },
    
    getDataType = function () { return (datatype) },
    
    getDimensionality = function () { return (length(voxdims)) },
    
    getDimensions = function () { return (imagedims) },
    
    getFieldOfView = function () { return (abs(voxdims) * imagedims) },
    
    getOrigin = function () { return (origin) },
    
    getSource = function () { return (source) },
    
    getStoredXformMatrix = function () { return (storedXform) },
    
    getTag = function (key)
    {
        if (!is.null(tags$keys) && !is.null(tags$values) && any(key == tags$keys))
        {
            rawValues <- tags$values[which(ket == tags$keys)]
            return (strsplit(implode(rawValues,sep=""), "\\s*\\\\\\s*", perl=TRUE))
        }
        else
            return (NA_character_)
    },
    
    getTags = function () { return (tags) },
    
    getVoxelDimensions = function () { return (voxdims) },
    
    getVoxelUnit = function () { return (voxunit) },
    
    isInternal = function () { return (source == "internal") },
    
    setSource = function (newSource)
    {
        if (is.character(newSource) && (length(newSource) == 1))
            .self$source <- newSource
    },
    
    summarise = function ()
    {
        if (length(datatype) == 0)
            datatypeString <- "undefined"
        else
        {
            datatypeString <- ifelse(datatype$isSigned, "signed", "unsigned")
            datatypeString <- paste(datatypeString, " ", datatype$type, ", ", datatype$size*8, " bits/voxel", sep="")
        }
        
        labels <- c("Image source", "Image dimensions", "Coordinate origin", "Voxel dimensions", "Data type", "Additional tags")
        values <- c(source, paste(implode(imagedims, sep=" x "),"voxels",sep=" "), paste("(",implode(round(origin,2), sep=","),")",sep=""), paste(implode(round(abs(voxdims),5), sep=" x "),ifelse(voxunit=="unknown","(units unknown)",voxunit),sep=" "), datatypeString, length(tags$keys))
        return (list(labels=labels, values=values))
    }
))

setClassUnion("SparseOrDenseArray", c("SparseArray","array"))

MriImage <- setRefClass("MriImage", contains="MriImageMetadata", fields=list(data="SparseOrDenseArray"), methods=list(
    initialize = function (data = array(), metadata = NULL, ...)
    {
        if (!is.null(metadata))
            import(metadata, "MriImageMetadata")
        else
            callSuper(...)
        return (initFields(data=data))
    },
    
    getData = function () { return (data) },
    
    getDataAtPoint = function (...)
    {
        dim <- getDimensionality()
        loc <- resolveVector(len=dim, ...)
        if (is.null(loc) || (length(...) != dim))
            report(OL$Error, "Point must be specified as a ", dim, "-vector")
            
        if (all(loc >= 1) && all(loc <= getDimensions()))
            return (data[matrix(loc,nrow=1)])
        else
            return (NA)
    },
    
    getMetadata = function () { return (export("MriImageMetadata")) },
    
    getSparseness = function ()
    {
        if (.self$isSparse())
            return (1 - (nrow(data$getCoordinates()) / prod(.self$getDimensions())))
        else
            return (sum(data==0 | is.na(data)) / prod(.self$getDimensions()))
    },
    
    isSparse = function () { return (is(data,"SparseArray")) },
    
    summarise = function ()
    {
        parentValue <- callSuper()
        sparseness <- paste(round(.self$getSparseness()*100,2), "% (", ifelse(.self$isSparse(),"sparse","dense"), " storage)", sep="")
        return (list(labels=c(parentValue$labels,"Sparseness"), values=c(parentValue$values,sparseness)))
    }
))

setAs("MriImage", "array", function (from) as(from$getData(),"array"))

setAs("array", "MriImage", function (from) {
    if (storage.mode(from) == "integer")
        datatype <- getDataTypeByNiftiCode(8)
    else if (storage.mode(from) == "double")
        datatype <- getDataTypeByNiftiCode(64)
    else
        report(OL$Error, "There is no MriImage data type to handle arrays of mode \"", storage.mode(from), "\"")
    
    nDims <- length(dim(from))
    origin <- c(1, 1, 1, 0, 0, 0)[1:nDims]
    metadata <- MriImageMetadata$new(imagedims=dim(from), voxdims=rep(1,nDims), datatype=datatype, origin=origin)
    image <- newMriImageWithData(from, metadata)
    
    return (image)
})

"[.MriImage" <- function (x, ..., drop = TRUE)
{
    return (x$getData()[...,drop=drop])
}

"[<-.MriImage" <- function (x, ..., value)
{
    x$data[...] <- value
    x$setSource("internal")
    return (x)
}

as.array.MriImage <- function (x, ...)
{
    as(x, "array")
}

dim.MriImage <- function (x)
{
    x$getDimensions()
}

Math.MriImage <- function (x, ...)
{
    newImage <- newMriImageWithSimpleFunction(x, .Generic, ...)
    return (newImage)
}

Ops.MriImage <- function (e1, e2)
{
    if (is(e2, "MriImage"))
        newImage <- newMriImageWithBinaryFunction(e1, e2, .Generic)
    else
        newImage <- newMriImageWithSimpleFunction(e1, .Generic, e2)
    return (newImage)
}

Summary.MriImage <- function (x, ..., na.rm = FALSE)
{
    if (nargs() > 2)
        report(OL$Error, "Function \"", .Generic, "\" is not defined for more than one image object")
    
    result <- get(.Generic)(x$getData())
    return (result)
}

setMethod("[", "MriImage", function (x, i, j, ..., drop = TRUE) {
    if (missing(j))
        return (x$getData()[i])
    else
        return (x$getData()[i,j,...,drop=drop])
})

setMethod("[<-", "MriImage", function (x, i, j, ..., value) {
    if (missing(j))
        x$data[i] <- value
    else
        x$data[i,j,...] <- value
    x$setSource("internal")
    return (x)
})

# setMethod("Math", "MriImage", Math.MriImage)

setMethod("Ops", "MriImage", Ops.MriImage)

setMethod("Summary", "MriImage", Summary.MriImage)

newMriImageWithDataRepresentation <- function (image, representation = c("dense","coordlist"))
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not an MriImage object")
    
    representation <- match.arg(representation)
    
    if (image$isSparse() && representation == "dense")
        newImage <- newMriImageWithData(as(image$getData(), "array"), image$getMetadata())
    else if (!image$isSparse() && representation == "coordlist")
        newImage <- newMriImageWithData(as(image$getData(), "SparseArray"), image$getMetadata())
    else
        newImage <- image
    
    invisible(newImage)
}

newMriImageMetadataFromTemplate <- function (metadata, imageDims = NA, voxelDims = NA, voxelUnit = NA, datatype = NA, origin = NA, tags = NA)
{
    if (!is(metadata, "MriImageMetadata"))
        report(OL$Error, "The specified metadata template is not valid")
    
    template <- metadata$serialise()
    params <- list(imagedims=imageDims,
                   voxdims=voxelDims,
                   voxunit=voxelUnit,
                   datatype=datatype,
                   origin=origin,
                   tags=tags)
    params <- params[!is.na(params)]
    
    composite <- c(params, template)
    composite <- composite[!duplicated(names(composite))]
    
    newMetadata <- MriImageMetadata$new(imagedims=composite$imagedims, voxdims=composite$voxdims, voxunit=composite$voxunit, datatype=composite$datatype, origin=composite$origin, tags=composite$tags)
    invisible (newMetadata)
}

newMriImageWithData <- function (data, metadata)
{
    # Do not use the metadata object supplied because it is mutable and we
    # don't want changes (to e.g. source) to affect multiple images
    metadataDuplicate <- newMriImageMetadataFromTemplate(metadata)
    image <- MriImage$new(data, metadataDuplicate)
    invisible (image)
}

newMriImageFromTemplate <- function (image, ...)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    oldMetadata <- image$getMetadata()
    newMetadata <- newMriImageMetadataFromTemplate(oldMetadata, ...)
    
    if (equivalent(oldMetadata$getDimensions(), newMetadata$getDimensions()))
        newImage <- MriImage$new(image$getData(), newMetadata)
    else
    {
        if (prod(oldMetadata$getDimensions()) != prod(newMetadata$getDimensions()))
            flag(OL$Warning, "The requested image dimensions will not result in a perfect reshaping")
        newData <- array(as.vector(image$getData()), dim=newMetadata$getDimensions())
        newImage <- MriImage$new(newData, newMetadata)
    }
    
    invisible (newImage)
}

newMriImageWithSimpleFunction <- function (image, fun, ..., newDataType = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    fun <- match.fun(fun)
    newData <- fun(image$getData(), ...)
    newDataType <- (if (is.null(newDataType)) image$getDataType() else newDataType)
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata(), datatype=newDataType)
    
    image <- MriImage$new(newData, metadata)
    invisible (image)
}

newMriImageWithBinaryFunction <- function (image1, image2, fun, ..., newDataType = NULL)
{
    if (!is(image1,"MriImage") || !is(image2,"MriImage"))
        report(OL$Error, "The specified images are not MriImage objects")
    
    fun <- match.fun(fun)
    newData <- fun(image1$getData(), image2$getData(), ...)
    newDataType <- (if (is.null(newDataType)) image1$getDataType() else newDataType)
    metadata <- newMriImageMetadataFromTemplate(image1$getMetadata(), datatype=newDataType)
    
    image <- MriImage$new(newData, metadata)
    invisible (image)
}

extractDataFromMriImage <- function (image, dim, loc)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() < max(2,dim))
        report(OL$Error, "The dimensionality of the specified image is too low")
    if (!(loc %in% 1:(image$getDimensions()[dim])))
        report(OL$Error, "The specified location is out of bounds")
    
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
    metadata <- image$getMetadata()$serialise()
    newMetadata <- MriImageMetadata$new(imagedims=metadata$imagedims[dimsToKeep], voxdims=metadata$voxdims[dimsToKeep], voxunit=metadata$voxunit, datatype=metadata$datatype, origin=metadata$origin[dimsToKeep], tags=metadata$tags)
        
    image <- MriImage$new(newData, newMetadata)
    invisible (image)
}

newMriImageByMasking <- function (image, mask)
{
    if (!identical(image$getDimensions(), dim(mask)))
        report(OL$Error, "The specified image and mask do not have the same dimensions")
    if (!is.logical(mask))
        report(OL$Error, "Mask must be specified as an array of logical values")
    
    newData <- image$getData() * mask
    metadata <- newMriImageMetadataFromTemplate(image$getMetadata())
    
    image <- MriImage$new(newData, metadata)
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
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    if (length(clearance) == 1)
        clearance <- rep(clearance, image$getDimensionality())
    
    data <- image$getData()
    dims <- image$getDimensions()
    indices <- lapply(seq_len(image$getDimensionality()), function (i) {
        dimMax <- apply(data, i, max)
        toKeep <- which(is.finite(dimMax) & dimMax > 0)
        if (length(toKeep) == 0)
            report(OL$Error, "Trimming the image would remove its entire contents")
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
