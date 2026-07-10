.EmptyMatrix <- matrix(NA, nrow=0, ncol=0)

#' The empty matrix
#' 
#' The empty matrix is a standard matrix of dimensions 0 x 0. It is intended to
#' be used as a placeholder where a matrix is required but no information is
#' stored.
#' 
#' @param object Any object.
#' @return \code{emptyMatrix} returns the empty matrix, equivalent to
#'   \code{matrix(NA,0,0)}. \code{is.emptyMatrix} returns \code{TRUE} if its
#'   argument is identical to the empty matrix.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
emptyMatrix <- function ()
{
    return (.EmptyMatrix)
}

#' @rdname emptyMatrix
#' @export
is.emptyMatrix <- function (object)
{
    return (identical(object, .EmptyMatrix))
}

#' The MriImage class
#' 
#' This class represents an MRI image. An object of this class is made up of
#' some voxel data, stored as a sparse or dense numeric array, and some
#' metadata, such as the file it was read from, the voxel dimensions, and so
#' on. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and
#' \code{\link{Summary}} are defined for this class, as are methods for
#' coercing to and from a standard \code{\link{array}}.
#'
#' This is a thin backwards-compatible shell around the value-semantics
#' \code{\link{mriImage}} S7 class, which holds the actual data and
#' implements all of the real logic. Every method here forwards to it.
#' 
#' @field imageDims Integer vector of dimensions
#' @field voxelDims Numeric vector of pixel/voxel spacings
#' @field voxelDimUnits Character vector of spatial and/or temporal spacing
#'   units. Millimetres and seconds (i.e., c("mm","s")) are typical
#' @field source String naming the file(s) that the image was read from. This
#'   is reset to the empty string if the image is modified
#' @field origin Numeric vector giving the spatial coordinate origin
#' @field xform Numeric matrix giving the NIfTI-style xform matrix associated
#'   with the image, which indicates its orientation
#' @field reordered Logical value indicating whether the image has been
#'   reordered. See \code{\link{reorderMriImage}}
#' @field tags Named list of arbitrary DICOM-style tags
#' @field data Sparse or dense array of data, or \code{NULL}
#' 
#' @export
MriImage <- setRefClass("MriImage", contains="SerialisableObject", fields=list(value.="mriImage", imageDims="integer", voxelDims="numeric", voxelDimUnits="character", source="character", origin="numeric", xform="matrix", reordered="logical", tags="list", data="ANY"), methods=list(
    # imageDims/voxelDims/.../data are a deliberate exception to the
    # single-hidden-field shell pattern: RNifti's C-level asNifti() duck-typing
    # accesses fields like $tags and $data directly (not via a generic
    # function), so they must be real, bare fields kept in sync with value.'s
    # properties. .syncFields() is called after every mutation to do this in
    # one place, so no call site can forget to update one of them.
    initialize = function (imageDims = NULL, voxelDims = NULL, voxelDimUnits = NULL, source = "", origin = NULL, xform = emptyMatrix(), reordered = TRUE, tags = list(), data = NULL, value. = NULL, ...)
    {
        if (!is.null(value.))
            object <- initFields(value.=value.)
        else
            object <- initFields(value.=newMriImage(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=voxelDimUnits, source=source, origin=origin, xform=xform, reordered=reordered, tags=tags, data=data, ...))
        object$.syncFields()
        return (object)
    },

    .syncFields = function ()
    {
        "(internal) Mirror value.'s properties onto real fields, for interop with code that expects direct field access (e.g. RNifti's C-level duck-typing)"
        .self$imageDims <- value.@imageDims
        .self$voxelDims <- value.@voxelDims
        .self$voxelDimUnits <- value.@voxelDimUnits
        .self$source <- value.@source
        .self$origin <- value.@origin
        .self$xform <- value.@xform
        .self$reordered <- value.@reordered
        .self$tags <- value.@tags
        .self$data <- .wrapIfSparseArray(value.@data)
        invisible (.self)
    },
    
    apply = function (...)
    {
        "Apply a function to the margins of the image"
        .mriImageApply(value., ...)
    },
    
    binarise = function ()
    {
        "Binarise the image by setting nonzero values to one"
        .self$value. <- tractor.base::imageBinarise(value.)
        .self$.syncFields()
        invisible (.self)
    },
    
    fill = function (value)
    {
        "Fill the image with a particular value"
        .self$value. <- .mriImageFill(value., value)
        .self$.syncFields()
        invisible (.self)
    },
    
    find = function (fun = NULL, ..., array = TRUE)
    {
        "Find voxels whose values are not zero, or satisfy a function"
        .mriImageFind(value., fun, ..., array=array)
    },
    
    getData = function () { return (.wrapIfSparseArray(values(value.))) },
    
    getDataAtPoint = function (...)
    {
        "Obtain the value of the image at a particular point"
        .mriImageGetDataAtPoint(value., ...)
    },
    
    getDimensionality = function () { return (.mriImageDimensionality(value.)) },
    
    getDimensions = function () { return (dim(value.)) },
    
    getFieldOfView = function () { return (.mriImageFieldOfView(value.)) },
    
    getMetadata = function ()
    {
        "Obtain a version of the image with any data removed"
        if (.self$isEmpty())
            return (.self$copy())
        else
            return (MriImage$new(value.=metadata(value.)))
    },
    
    getNonzeroIndices = function (array = TRUE, positiveOnly = FALSE)
    {
        "Find voxels whose values are not zero"
        .mriImageGetNonzeroIndices(value., array=array, positiveOnly=positiveOnly)
    },
    
    getOrigin = function () { return (value.@origin) },
    
    getSlice = function (dim, loc)
    {
        "Extract data from a slice of the image along one dimension"
        .mriImageGetSlice(value., dim, loc)
    },
    
    getSource = function () { return (value.@source) },
    
    getSparseness = function ()
    {
        "Obtain the proportion of zeroes in the image"
        .mriImageGetSparseness(value.)
    },
    
    getTags = function (keys = NULL)
    {
        "Retrieve some or all of the tags stored with the image"
        .mriImageGetTags(value., keys)
    },
    
    getVoxelDimensions = function () { return (pixdim(value.)) },
    
    getVoxelUnits = function () { return (pixunits(value.)) },
    
    getXform = function (implicit = TRUE)
    {
        "Retrieve the stored or implicit xform matrix"
        .mriImageGetXform(value., implicit=implicit)
    },
    
    hasTags = function (keys) { return (.mriImageHasTags(value., keys)) },
    
    isEmpty = function () { return (.mriImageIsEmpty(value.)) },
    
    isInternal = function () { return (.mriImageIsInternal(value.)) },
    
    isReordered = function () { return (.mriImageIsReordered(value.)) },
    
    isRgb = function () { return (.mriImageIsRgb(value.)) },
    
    isSparse = function () { return (.mriImageIsSparse(value.)) },
    
    map = function (fun, ..., sparse = NULL)
    {
        "Replace the current data with the result of a function"
        args <- lapply(list(...), function(a) if (is(a,"MriImage")) a$value. else a)
        .self$value. <- do.call(imageMap, c(list(value.,fun), args, list(sparse=sparse)))
        .self$.syncFields()
        invisible (.self)
    },
    
    mask = function (maskImage)
    {
        "Mask the image, setting zero voxels in the mask to zero"
        .self$value. <- tractor.base::mask(value., maskImage$value.)
        .self$.syncFields()
        invisible (.self)
    },
    
    nChannels = function () { return (.mriImageNChannels(value.)) },
    
    nSlices = function () { return (.mriImageNSlices(value.)) },
    
    nTags = function () { return (.mriImageNTags(value.)) },
    
    nVolumes = function () { return (.mriImageNVolumes(value.)) },

    threshold = function (level, defaultValue = 0)
    {
        "Threshold the image by setting values below the threshold level to zero"
        .self$value. <- tractor.base::imageThreshold(value., level, defaultValue)
        .self$.syncFields()
        invisible (.self)
    },
    
    setData = function (newData)
    {
        "Replace the data in the image"
        if (is.null(newData) || equivalent(dim(newData),dim(value.)))
        {
            .self$value. <- `values<-`(value., newData)
            .self$.syncFields()
        }
        else
            flag(OL$Warning, "New data does not match the image dimensions")
        invisible(.self)
    },
    
    setOrigin = function (newOrigin)
    {
        "Update the origin of the image"
        if (is.numeric(newOrigin) && length(newOrigin) == .self$getDimensionality())
        {
            .self$value. <- set_props(value., origin=newOrigin, source="")
            .self$.syncFields()
        }
        invisible(.self)
    },
    
    setSource = function (newSource)
    {
        "Update the source of the image"
        if (is.null(newSource))
            .self$value. <- set_props(value., source="")
        else if (is.character(newSource) && (length(newSource) == 1))
            .self$value. <- set_props(value., source=newSource)
        .self$.syncFields()
        invisible(.self)
    },
    
    setTags = function (..., merge = FALSE)
    {
        "Add, replace or merge metadata tags"
        .self$value. <- .mriImageSetTags(value., ..., merge=merge)
        .self$.syncFields()
        invisible(.self)
    },
    
    setXform = function (newXform)
    {
        "Update the xform matrix associated with the image"
        if (is.matrix(newXform) && equivalent(dim(newXform),c(4,4)))
        {
            .self$value. <- set_props(value., xform=newXform, source="")
            .self$.syncFields()
        }
        invisible(.self)
    },
    
    serialise = function (file = NULL)
    {
        "Serialise the object to a list or file"
        fields <- list(imageDims=dim(value.), voxelDims=pixdim(value.), voxelDimUnits=pixunits(value.), source=value.@source, origin=value.@origin, xform=.mriImageGetXform(value.,implicit=FALSE), reordered=value.@reordered, tags=value.@tags, data=values(value.))
        out <- structure(fields, originalClass="MriImage", originalPackage="tractor.base")
        if (!is.null(file))
            save(out, file=ensureFileSuffix(file,"Rdata"))
        invisible (out)
    },

    summarise = function ()
    {
        .mriImageSummarise(value.)
    },
    
    writeToFile = function (...) { writeImageFile(.self, ...) }
))

# Register deserialiser for MriImageMetadata legacy class
registerDeserialiser("MriImageMetadata", function (fields, ...) {
    object <- MriImage$new(imageDims=fields$imagedims, voxelDims=fields$voxdims, voxelDimUnits=fields$voxunit, source=fields$source, origin=fields$origin, xform=fields$storedXform, tags=fields$tags, data=NULL)
    return (object)
})

setAs("MriImage", "array", function (from) as.array(from$value.))

setAs("array", "MriImage", function (from) asMriImage(from))

setAs("character", "MriImage", function (from) readImageFile(from))

.warnIfIndexingUnreorderedImage <- function (image)
{
    # The argument is an unreordered image and contains a non-LAS xform
    if (is(image,"MriImage") && !image$isReordered() && orientation(image) != "LAS")
        flag(OL$Warning, "Indexing into an image which is not reordered has no consistent meaning")
}

.wrapIfSparseArray <- function (x)
{
    # Legacy callers expect a SparseArray shell (with $-style methods), not
    # the new value-semantics sparseArray S7 class, wherever sparse data
    # crosses the MriImage legacy API boundary
    if (is(x, "sparseArray"))
        return (SparseArray$new(value.=x))
    else
        return (x)
}

#' @export
as.array.MriImage <- function (x, ...)
{
    as.array(x$value.)
}

#' @export
dim.MriImage <- function (x)
{
    x$getDimensions()
}

#' @export
Math.MriImage <- function (x, ...)
{
    MriImage$new(value.=imageMap(x$value., .Generic))
}

#' @export
Ops.MriImage <- function (e1, e2)
{
    e2v <- if (is(e2,"MriImage")) e2$value. else e2
    MriImage$new(value.=imageMap(e1$value., .Generic, e2v))
}

#' @export
Summary.MriImage <- function (x, ..., na.rm = FALSE)
{
    if (nargs() > 2)
        report(OL$Error, "Function \"#{.Generic}\" is not defined for more than one image object")
    
    result <- get(.Generic)(x$getData(),na.rm=na.rm)
    return (result)
}

#' Indexing methods for the legacy MriImage class
#'
#' These are the backwards-compatible \code{[}/\code{[<-} methods for the
#' legacy \code{\linkS4class{MriImage}} shell.
#'
#' @param x An \code{MriImage} object.
#' @param i,j,\dots Indexing objects.
#' @param drop Scalar value: should unitary dimensions be dropped?
#' @param value New value(s) for replacement forms.
#' @return A vector, array or \code{\linkS4class{SparseArray}}.
#' @rdname index-legacy-mri
#' @export
setMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 2)
        return (x$getData())
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, NULL, ...)
        return (.wrapIfSparseArray(x$value.@data[indices,drop=drop]))
    }
    else
        return (.wrapIfSparseArray(x$value.@data[,,...,drop=drop]))
})

#' @rdname index-legacy-mri
#' @export
setMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 3)
        return (.wrapIfSparseArray(x$value.@data[i,drop=drop]))
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, NULL, ...)
        return (.wrapIfSparseArray(x$value.@data[indices,drop=drop]))
    }
    else
        return (.wrapIfSparseArray(x$value.@data[i,,...,drop=drop]))
})

#' @rdname index-legacy-mri
#' @export
setMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, j, ...)
        return (.wrapIfSparseArray(x$value.@data[indices,drop=drop]))
    }
    else
        return (.wrapIfSparseArray(x$value.@data[,j,...,drop=drop]))
})

#' @rdname index-legacy-mri
#' @export
setMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        return (.wrapIfSparseArray(x$value.@data[indices,drop=drop]))
    }
    else
        return (.wrapIfSparseArray(x$value.@data[i,j,...,drop=drop]))
})

#' @rdname index-legacy-mri
#' @export
setMethod("[", signature(x="MriImage",i="MriImage",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    return (x[i$getNonzeroIndices(array=TRUE,...), drop=drop])
})

#' @rdname index-legacy-mri
#' @export
setReplaceMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - 1
    newData <- x$value.@data
    if (nArgs < 2)
        newData[] <- value
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        newData[indices] <- value
    }
    else
        newData[,,...] <- value
    x$setData(newData)
    x$setSource(NULL)
    return (x)
})

#' @rdname index-legacy-mri
#' @export
setReplaceMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - 1
    newData <- x$value.@data
    if (nArgs < 3)
        newData[i] <- value
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, NULL, ...)
        newData[indices] <- value
    }
    else
        newData[i,,...] <- value
    x$setData(newData)
    x$setSource(NULL)
    return (x)
})

#' @rdname index-legacy-mri
#' @export
setReplaceMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    newData <- x$value.@data
    if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, j, ...)
        newData[indices] <- value
    }
    else
        newData[,j,...] <- value
    x$setData(newData)
    x$setSource(NULL)
    return (x)
})

#' @rdname index-legacy-mri
#' @export
setReplaceMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    newData <- x$value.@data
    if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        newData[indices] <- value
    }
    else
        newData[i,j,...] <- value
    x$setData(newData)
    x$setSource(NULL)
    return (x)
})

#' @rdname index-legacy-mri
#' @export
setReplaceMethod("[", signature(x="MriImage",i="MriImage",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    newData <- x$value.@data
    newData[i$getNonzeroIndices(array=TRUE,...)] <- value
    x$setData(newData)
    x$setSource(NULL)
    return (x)
})

# setMethod("Math", "MriImage", Math.MriImage)

setMethod("Ops", "MriImage", Ops.MriImage)

setMethod("Summary", "MriImage", Summary.MriImage)

#' Creating MriImage objects from data
#' 
#' Functions for creating MriImage objects from data, including other images.
#' All of these functions use data from arrays or \code{MriImage} objects to
#' create a new \code{MriImage} object. \code{asMriImage} is the basic fucntion
#' for creating an object from its constituents: an array of voxel values and
#' some metadata (and/or a template image).
#' 
#' \code{extractMriImage} reduces the dimensionality of the source image by
#' one, by extracting a single ``slice'' of data along one dimension.
#' \code{trimMriImage} trims empty space from the edges of an image, reducing
#' the dimensions of the image and thus avoiding the storage of lots of zeroes.
#' \code{reorderMriImage} reorders the image data (and corresponding metadata)
#' to the LAS convention, an operation which is usually performed when an
#' image is read from file.
#' 
#' @param data An array of pixel/voxel data.
#' @param templateImage An optional \code{MriImage} object, to be used as a
#'   metadata template.
#' @param imageDims,voxelDims,voxelDimUnits,origin,tags,reordered Metadata for
#'   the new image object. These values override any from the metadata object
#'   or data array. See \code{\linkS4class{MriImage}} class documentation for
#'   details.
#' @param image An \code{MriImage} object.
#' @param dim,loc The dimension and location along that dimension for which
#'   data should be extracted.
#' @param clearance The number of voxels' clearance left around a trimmed
#'   image.
#' @param indices A list of indices to keep along each dimension. Determined
#'   from the specified \code{clearance} if \code{NULL}.
#' @return An \code{MriImage} object.
#' 
#' @author Jon Clayden
#' @seealso \code{\linkS4class{MriImage}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
asMriImage <- function (data, templateImage = nilObject(), imageDims = NA, voxelDims = NA, voxelDimUnits = NA, origin = NA, tags = NA, reordered = NA)
{
    # NB: Be careful when changing the behaviour of this function
    # Quite a bit of other code relies on various aspects of its semantics
    if (is.null(data))
        report(OL$Error, "Data may not be NULL")
    if (is.logical(data))
        data <- as.integer(data)
    if (!is.numeric(data) && !is(data,"SparseArray"))
        report(OL$Error, "The specified data is not numeric")
    
    if (!identical(imageDims, NA))
        nDims <- length(imageDims)
    else if (!is.null(dim(data)))
    {
        nDims <- length(dim(data))
        imageDims <- dim(data)
    }
    else if (is(templateImage, "MriImage"))
    {
        subspaceDims <- cumprod(templateImage$getDimensions())
        nDims <- which(subspaceDims == length(data))
        if (length(nDims) == 0)
            report(OL$Error, "Dimensionality of the data cannot be guessed from the template")
        else
        {
            nDims <- nDims[1]
            imageDims <- templateImage$getDimensions()[1:nDims]
        }
    }
    else
        report(OL$Error, "No information on image dimensions is available")
    
    defaults <- list(voxelDims=rep(1,nDims), voxelDimUnits="unknown", origin=c(1,1,1), xform=emptyMatrix(), tags=list(), reordered=TRUE)
    template <- templateImage$serialise()
    params <- list(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=voxelDimUnits, origin=origin, tags=tags, reordered=reordered)
    params <- params[!is.na(params)]
    
    composite <- deduplicate(params, template, defaults)
    
    image <- MriImage$new(imageDims=composite$imageDims[1:nDims], voxelDims=composite$voxelDims[1:nDims], voxelDimUnits=composite$voxelDimUnits, origin=composite$origin, xform=composite$xform, reordered=composite$reordered, tags=composite$tags, data=data)
    invisible (image)
}

#' @rdname asMriImage
#' @export
extractMriImage <- function (image, dim, loc)
{
    image <- as(image, "MriImage")
    
    newData <- image$getSlice(dim, loc)
    dimsToKeep <- setdiff(1:image$getDimensionality(), dim)
    
    image <- MriImage$new(imageDims=image$getDimensions()[dimsToKeep], voxelDims=image$getVoxelDimensions()[dimsToKeep], voxelDimUnits=image$getVoxelUnits(), origin=image$getOrigin(), xform=image$getXform(implicit=FALSE), reordered=image$isReordered(), tags=image$getTags(), data=newData)
    return (image)
}

#' @rdname asMriImage
#' @export
trimMriImage <- function (image, clearance = 4, indices = NULL)
{
    image <- as(image, "MriImage")
    
    if (length(clearance) == 1)
        clearance <- rep(clearance, image$getDimensionality())
    
    data <- image$getData()
    dims <- image$getDimensions()
    if (is.null(indices))
    {
        indices <- lapply(seq_len(image$getDimensionality()), function (i) {
            dimMax <- suppressWarnings(apply(data, i, max, na.rm=TRUE))
            toKeep <- which(is.finite(dimMax) & dimMax > 0)
            if (length(toKeep) == 0)
                report(OL$Error, "Trimming the image would remove its entire contents")
            minLoc <- max(1, min(toKeep)-clearance[i])
            maxLoc <- min(dims[i], max(toKeep)+clearance[i])
            return (minLoc:maxLoc)
        })
    }
    
    data <- do.call("[", c(list(data),indices,list(drop=FALSE)))
    newDims <- sapply(indices, length)
    
    # NB: Origin is not corrected here
    newImage <- structure(asMriImage(data,image,imageDims=newDims), indices=indices)
    invisible (newImage)
}

#' @rdname asMriImage
#' @export
reorderMriImage <- function (image)
{
    if (is(image,"MriImage") && image$isReordered())
        return (image)
    else
    {
        image <- retrieveNifti(image)
        orientation(image) <- "LAS"
        return (as(structure(image,reordered=TRUE), "MriImage"))
    }
}

#' Merging MriImage objects
#' 
#' This function concatenates the data from a series of \code{MriImage}
#' objects, and then attempts to work out the final dimensions of the merged
#' image and returns it.
#' 
#' @param ... \code{MriImage} objects. They do not need to have the same
#'   dimensionality.
#' @param bindDim An integer specifying the dimension along which to bind the
#'   data, or \code{NULL} (the default). The latter case resolves to one number
#'   higher than the last dimension common to all images.
#' @param padTags Logical value. If \code{TRUE}, \code{NA}s will be used to pad
#'   tags which appear to be partially missing in the merged dataset. If
#'   \code{FALSE}, incomplete tags will be dropped.
#' @return A merged image.
#' 
#' @note Tags are retained as-is if they are identical in each image. Otherwise
#'   they are concatenated if their lengths match the number of blocks in each
#'   image, or concatenated with NAs for missing values if \code{padTags} is
#'   \code{TRUE}.
#' @author Jon Clayden
#' @seealso \code{\linkS4class{MriImage}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
mergeMriImages <- function (..., bindDim = NULL, padTags = FALSE)
{
    images <- lapply(list(...), as, "MriImage")
    if (length(images) == 1)
        return (images[[1]])
    if (!allEqual(sapply(images, orientation)))
        images <- lapply(images, reorderMriImage)
    if (!allEqual(lapply(images, xform), tolerance=1e-4, check.attributes=FALSE))
        report(OL$Warning, "Merging images with nonequal xforms - this is probably unwise")
    
    dimensionalities <- sapply(images, function(x) x$getDimensionality())
    lastDim <- max(dimensionalities, bindDim)
    dimensions <- sapply(seq_along(images), function(i) c(images[[i]]$getDimensions(), rep(1L,lastDim-dimensionalities[i])))
    
    commonDims <- apply(dimensions, 1, allEqual)
    assert(commonDims[1], "Images must have at least their first dimension in common")
    if (is.null(bindDim))
        bindDim <- max(which(commonDims)) + 1L
    if (bindDim > lastDim)
        dimensions <- rbind(dimensions, matrix(1,nrow=(bindDim-lastDim),ncol=ncol(dimensions)))
    blockDims <- dimensions[seq_len(bindDim-1),1]
    blockCounts <- apply(dimensions[bindDim:lastDim,,drop=FALSE], 2, prod)
    
    imageSizes <- apply(dimensions, 2, prod, na.rm=TRUE)
    data <- do.call("c", lapply(images, as.array))
    dim(data) <- c(blockDims, length(data) %/% prod(blockDims))
    
    resolveValue <- function (value, requiredLength)
    {
        if (is.null(value))
            rep(NA, requiredLength)
        else if (length(value) == requiredLength)
            value
        else if (nrow(promote(value,TRUE)) == requiredLength)
            promote(value, TRUE)
        else if (length(value) == 1)
            rep(value, requiredLength)
        else
            NULL
    }
    
    tags <- lapply(seq_along(images), function(i) c(list(.blocks=blockCounts[i]), images[[i]]$getTags()))
    tagNames <- Reduce(ifelse(padTags,union,intersect), lapply(tags, names))
    tags <- Reduce(function(x,y) {
        sapply(tagNames, function(n) {
            if (n == ".blocks")
                x[[n]] + y[[n]]
            else if (equivalent(x[[n]], y[[n]]))
                x[[n]]
            else
            {
                xn <- resolveValue(x[[n]], x$.blocks)
                yn <- resolveValue(y[[n]], y$.blocks)
                if (is.matrix(xn) && !is.matrix(yn) && all(is.na(yn)))
                    yn <- matrix(NA, nrow=length(yn), ncol=ncol(xn))
                if (is.matrix(yn) && !is.matrix(xn) && all(is.na(xn)))
                    xn <- matrix(NA, nrow=length(xn), ncol=ncol(yn))
                if (is.matrix(xn) || is.matrix(yn))
                    rbind(xn, yn)
                else if (is.null(xn) || is.null(yn))
                    NULL
                else
                    c(xn, yn)
            }
        }, simplify=FALSE, USE.NAMES=TRUE)
    }, tags)
    
    tags$.blocks <- NULL
    
    return (asMriImage(data, images[[which.max(imageSizes)]], tags=tags[!sapply(tags,is.null)]))
}
