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
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
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

setClassUnion("MriImageData", c("SparseArray","array","NULL"))

#' The MriImage class
#' 
#' This class represents an MRI image. An object of this class is made up of
#' some voxel data, stored as a sparse or dense numeric array, and some
#' metadata, such as the file it was read from, the voxel dimensions, and so
#' on. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and
#' \code{\link{Summary}} are defined for this class, as are methods for
#' coercing to and from a standard \code{\link{array}}.
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
MriImage <- setRefClass("MriImage", contains="SerialisableObject", fields=list(imageDims="integer",voxelDims="numeric",voxelDimUnits="character",source="character",origin="numeric",xform="matrix",reordered="logical",tags="list",data="MriImageData"), methods=list(
    initialize = function (imageDims = NULL, voxelDims = NULL, voxelDimUnits = NULL, source = "", origin = NULL, xform = emptyMatrix(), reordered = TRUE, tags = list(), data = NULL, ...)
    {
        oldFields <- list(...)
        
        # Resolve image dimensions
        .dims <- imageDims
        if (is.null(imageDims))
        {
            if ("imagedims" %in% names(oldFields))
                .dims <- oldFields$imagedims
            else
                .dims <- dim(data)
        }
        
        # Sanity checking
        if (!is.null(.dims) && length(.dims) < 2)
            report(OL$Error, "Image must be multidimensional")
        if (!is.null(data) && !is.null(dim(data)) && !equivalent(.dims,dim(data)))
            flag(OL$Warning, "Data dimensions do not match the specified image dimensions")
        
        # Resolve voxel dimensions and associated units
        .voxdims <- voxelDims
        .voxunits <- as.character(voxelDimUnits)
        if (is.null(voxelDims) && "voxdims" %in% names(oldFields))
            .voxdims <- oldFields$voxdims
        if (is.null(voxelDimUnits) && "voxunit" %in% names(oldFields))
            .voxunits <- as.character(oldFields$voxunit)
        
        # Name voxel units
        names(.voxunits)[.voxunits %~% "m$"] <- "spatial"
        names(.voxunits)[.voxunits %~% "s$"] <- "temporal"
        
        # Resolve xform
        .xform <- xform
        if (is.emptyMatrix(xform) && "storedXform" %in% names(oldFields))
            .xform <- oldFields$storedXform
        
        if (length(.dims) != length(.voxdims))
            report(OL$Error, "Image and voxel dimensions should have the same length")
        
        # Don't call as.character() on .voxunits here, as it will drop names
        object <- initFields(imageDims=as.integer(.dims), voxelDims=abs(as.numeric(.voxdims)), voxelDimUnits=.voxunits, source=as.character(source), origin=as.numeric(origin), xform=as.matrix(.xform), reordered=reordered, tags=tags, data=data)
        
        # Make sure the data dimensions are right
        if (!is.null(object$data))
            dim(object$data) <- .dims
        
        # Various fix-ups for backwards compatibility
        if (length(object$voxelDimUnits) == 0)
            object$voxelDimUnits <- "unknown"
        if (length(object$source) != 1 || object$source == "internal")
            object$source <- ""
        if (length(object$tags) == 2 && all(c("keys","values") %in% names(object$tags)))
            object$tags <- structure(as.list(object$tags$values), names=object$tags$keys)
        if (length(object$origin) < 3)
            object$origin <- c(object$origin, rep(0,3-length(object$origin)))
        else
            object$origin <- object$origin[1:3]
        
        return (object)
    },
    
    apply = function (...)
    {
        "Apply a function to the margins of the image"
        if (.self$isEmpty())
            report(OL$Error, "The image contains no data")
        else if (.self$isSparse())
            return (data$apply(...))
        else
            return (base::apply(data, ...))
    },
    
    binarise = function ()
    {
        "Binarise the image by setting nonzero values to one"
        .self$map(function(x) ifelse(x!=0, 1L, 0L))
    },
    
    fill = function (value)
    {
        "Fill the image with a particular value"
        if (value == 0)
            .self$setData(newSparseArrayWithData(vector(class(value),0), matrix(NA,0,length(imageDims)), imageDims))
        else
            .self$setData(array(value, dim=imageDims))
    },
    
    find = function (fun = NULL, ..., array = TRUE)
    {
        "Find voxels whose values are not zero, or satisfy a function"
        .warnIfIndexingUnreorderedImage(.self)
        
        if (.self$isEmpty())
            return (integer(0))
        else if (is.null(fun))
        {
            if (.self$isSparse())
            {
                locs <- data$getCoordinates()
                if (array)
                    return (locs)
                else
                    return (matrixToVectorLocs(locs, imageDims))
            }
            else
                return (which(data != 0, arr.ind=array))
        }
        else
        {
            if (is.numeric(fun) && length(fun) == 1)
                locs <- which(as.array(data) == fun)
            else
            {
                fun <- match.fun(fun)
                locs <- which(as.logical(fun(as.array(data), ...)))
            }
            
            if (array)
                return (vectorToMatrixLocs(locs, imageDims))
            else
                return (locs)
        }
    },
    
    getData = function () { return (data) },
    
    getDataAtPoint = function (...)
    {
        "Obtain the value of the image at a particular point"
        if (is.null(data))
            return (NA)
        
        .warnIfIndexingUnreorderedImage(.self)
        
        dim <- getDimensionality()
        loc <- resolveVector(len=dim, ...)
        if (is.null(loc) || (length(loc) != dim))
            report(OL$Error, "Point must be specified as a ", dim, "-vector")
            
        if (all(loc >= 1) && all(loc <= getDimensions()))
            return (data[matrix(loc,nrow=1)])
        else
            return (NA)
    },
    
    getDimensionality = function () { return (length(voxelDims)) },
    
    getDimensions = function () { return (imageDims) },
    
    getFieldOfView = function () { return (abs(voxelDims) * imageDims) },
    
    getMetadata = function ()
    {
        "Obtain a version of the image with any data removed"
        if (.self$isEmpty())
            return (.self$copy())
        else
            return (MriImage$new(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=voxelDimUnits, source=source, origin=origin, xform=xform, reordered=reordered, tags=tags, data=NULL))
    },
    
    getNonzeroIndices = function (array = TRUE, positiveOnly = FALSE)
    {
        "Find voxels whose values are not zero"
        .warnIfIndexingUnreorderedImage(.self)
        
        if (positiveOnly)
            return (.self$find(fx(x > 0), array=array))
        else
            return (.self$find(array=array))
    },
    
    getOrigin = function () { return (origin) },
    
    getSlice = function (dim, loc)
    {
        "Extract data from a slice of the image along one dimension"
        if (length(imageDims) < max(2,dim))
            report(OL$Error, "The dimensionality of the image is too low")
        if (!(loc %in% seq_len(imageDims[dim])))
            report(OL$Error, "The specified location is out of bounds")
        
        .warnIfIndexingUnreorderedImage(.self)
        
        dimsToKeep <- setdiff(seq_along(imageDims), dim)
        if (.self$isEmpty())
            newData <- NULL
        else if (.self$isSparse())
        {
            # This code is faster when working with a sparse array
            newData <- array(0, dim=imageDims[dimsToKeep])
            matchingCoords <- which(data$getCoordinates()[,dim] == loc)
            newData[data$getCoordinates()[matchingCoords,dimsToKeep,drop=FALSE]] <- data$getData()[matchingCoords]
        }
        else
        {
            indices <- alist(i=,j=,k=,t=,u=,v=,w=)[seq_along(imageDims)]
            indices[dim] <- loc
            newData <- do.call("[", c(list(data),indices))
            if (is.vector(newData))
                newData <- promote(newData)
        }
        
        invisible(newData)
    },
    
    getSource = function () { return (source) },
    
    getSparseness = function ()
    {
        "Obtain the proportion of zeroes in the image"
        if (.self$isEmpty())
            return (NA)
        else if (.self$isSparse())
            return (1 - (nrow(data$getCoordinates()) / prod(.self$getDimensions())))
        else
            return (sum(data==0 | is.na(data)) / prod(.self$getDimensions()))
    },
    
    getTags = function (keys = NULL)
    {
        "Retrieve some or all of the tags stored with the image"
        indexList(tags, keys)
    },
    
    getVoxelDimensions = function () { return (voxelDims) },
    
    getVoxelUnits = function () { return (voxelDimUnits) },
    
    getXform = function (implicit = TRUE)
    {
        "Retrieve the stored or implicit xform matrix"
        if (is.emptyMatrix(.self$xform) && .self$isReordered() && implicit)
        {
            implicitXform <- diag(4)
            zeroBasedOrigin <- pmax(origin-1, c(0,0,0))
            if (.self$getDimensionality() == 2)
            {
                implicitXform[c(1,6)] <- c(-1,1) * abs(voxelDims)
                zeroBasedOrigin[1:2] <- zeroBasedOrigin[1:2] * abs(voxelDims)
            }
            else
            {
                implicitXform[c(1,6,11)] <- c(-1,1,1) * abs(voxelDims[1:3])
                zeroBasedOrigin <- zeroBasedOrigin * abs(voxelDims[1:3])
            }
            implicitXform[1:3,4] <- c(1,-1,-1) * zeroBasedOrigin
            return (implicitXform)
        }
        else
            return (.self$xform)
    },
    
    hasTags = function (keys) { return (sapply(keys, function(key) !is.null(tags[[key]]))) },
    
    isEmpty = function () { return (is.null(data)) },
    
    isInternal = function () { return (source == "") },
    
    isReordered = function () { return (isTRUE(reordered)) },
    
    isSparse = function () { return (is(data,"SparseArray")) },
    
    map = function (fun, ..., sparse = NULL)
    {
        "Replace the current data with the result of a function"
        args <- lapply(list(.self,...), function(x) {
            if (is(x, "MriImage"))
                return (as.array(x$getData()))
            else
                return (x)
        })
        result <- do.call(fun, args)
        
        if (!equivalent(dim(result), imageDims))
            dim(result) <- imageDims
        
        if (is.null(sparse))
            sparse <- (sum(result==0) / length(result) >= 0.75)
        if (isTRUE(sparse))
            result <- as(result, "SparseArray")
        
        .self$setData(result)
    },
    
    mask = function (maskImage)
    {
        "Mask the image, setting zero voxels in the mask to zero"
        .self$map(function(x,y) ifelse(y==0,0,x), maskImage)
    },
    
    nSlices = function () { return (ifelse(length(imageDims) > 2, imageDims[3], 1L)) },
    
    nTags = function () { return (length(tags)) },
    
    nVolumes = function () { return (ifelse(length(imageDims) > 3, prod(imageDims[-(1:3)]), 1L)) },
    
    setData = function (newData)
    {
        "Replace the data in the image"
        if (is.null(newData) || equivalent(dim(newData),imageDims))
        {
            .self$data <- newData
            .self$setSource(NULL)
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
            .self$origin <- newOrigin
            .self$setSource(NULL)
        }
        invisible(.self)
    },
    
    setSource = function (newSource)
    {
        "Update the source of the image"
        if (is.null(newSource))
            .self$source <- ""
        else if (is.character(newSource) && (length(newSource) == 1))
            .self$source <- newSource
        invisible(.self)
    },
    
    setTags = function (...)
    {
        "Add or replace metadata tags"
        newTags <- deduplicate(list(...), .self$tags)
        .self$tags <- newTags[!sapply(newTags,is.null)]
        invisible(.self)
    },
    
    setXform = function (newXform)
    {
        "Update the xform matrix associated with the image"
        if (is.matrix(newXform) && equivalent(dim(newXform),c(4,4)))
        {
            .self$xform  <- newXform
            .self$setSource(NULL)
        }
        invisible(.self)
    },
    
    summarise = function ()
    {
        spatialUnit <- voxelDimUnits["spatial"]
        temporalUnit <- voxelDimUnits["temporal"]
        voxelDimString <- paste(implode(round(abs(voxelDims[1:min(3,length(voxelDims))]),5), sep=" x "), ifelse(!is.na(spatialUnit),paste(" ",spatialUnit,sep=""),""), sep="")
        if (length(voxelDims) > 3)
            voxelDimString <- paste(voxelDimString, " x ", round(abs(voxelDims[4]),5), ifelse(!is.na(spatialUnit) && !is.na(temporalUnit),paste(" ", temporalUnit,sep=""),""), sep="")
        if (length(voxelDims) > 4)
            voxelDimString <- paste(voxelDimString, " x ", implode(round(abs(voxelDims[5:length(voxelDims)]),5), sep=" x "), sep="")
        if (all(voxelDimUnits == "unknown"))
            voxelDimString <- paste(voxelDimString, "(units unknown)", sep=" ")
        
        tagNames <- names(tags)
        if (length(tagNames) == 0)
            tagNames <- "(none)"
        
        labels <- c("Image source", "Image dimensions", "Voxel dimensions", "Coordinate origin", "Additional tags")
        values <- c(ifelse(source=="","internal",source), paste(implode(imageDims, sep=" x "),"voxels",sep=" "), voxelDimString, paste("(",implode(round(origin,2), sep=","),")",sep=""), implode(tagNames,sep=", "))
        
        if (!.self$isEmpty())
        {
            sparseness <- paste(round(.self$getSparseness()*100,2), "% (", ifelse(.self$isSparse(),"sparse","dense"), " storage)", sep="")
            labels <- c(labels, "Sparseness")
            values <- c(values, sparseness)
        }
        
        return (list(labels=labels, values=values))
    },
    
    threshold = function (level, defaultValue = 0)
    {
        "Threshold the image by setting values below the threshold level to zero"
        .self$map(function(x) ifelse(x >= level, x, defaultValue))
    },
    
    writeToFile = function (...) { writeImageFile(.self, ...) }
))

# Register deserialiser for MriImageMetadata legacy class
registerDeserialiser("MriImageMetadata", function (fields) {
    object <- MriImage$new(imageDims=fields$imagedims, voxelDims=fields$voxdims, voxelDimUnits=fields$voxunit, source=fields$source, origin=fields$origin, xform=fields$storedXform, tags=fields$tags, data=NULL)
    return (object)
})

setAs("MriImage", "array", function (from) as(from$getData(),"array"))

setAs("array", "MriImage", function (from) asMriImage(from))

.warnIfIndexingUnreorderedImage <- function (image)
{
    # The argument is an unreordered image and contains a non-LAS xform
    if (is(image,"MriImage") && !image$isReordered() && orientation(image) != "LAS")
        flag(OL$Warning, "Indexing into an image which is not reordered has no consistent meaning")
}

#' @export
as.array.MriImage <- function (x, ...)
{
    as(x, "array")
}

#' @export
dim.MriImage <- function (x)
{
    x$getDimensions()
}

#' @export
Math.MriImage <- function (x, ...)
{
    x$copy()$map(.Generic)
}

#' @export
Ops.MriImage <- function (e1, e2)
{
    e1$copy()$map(.Generic, e2)
}

#' @export
Summary.MriImage <- function (x, ..., na.rm = FALSE)
{
    if (nargs() > 2)
        report(OL$Error, "Function \"#{.Generic}\" is not defined for more than one image object")
    
    result <- get(.Generic)(x$getData(),na.rm=na.rm)
    return (result)
}

#' @rdname index
#' @export
setMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 2)
        return (x$data)
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, NULL, ...)
        return (x$data[indices,drop=drop])
    }
    else
        return (x$data[,,...,drop=drop])
})

#' @rdname index
#' @export
setMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 3)
        return (x$data[i,drop=drop])
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, NULL, ...)
        return (x$data[indices,drop=drop])
    }
    else
        return (x$data[i,,...,drop=drop])
})

#' @rdname index
#' @export
setMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, j, ...)
        return (x$data[indices,drop=drop])
    }
    else
        return (x$data[,j,...,drop=drop])
})

#' @rdname index
#' @export
setMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        return (x$data[indices,drop=drop])
    }
    else
        return (x$data[i,j,...,drop=drop])
})

#' @rdname index
#' @export
setMethod("[", signature(x="MriImage",i="MriImage",j="missing"), function (x, i, j, ..., drop = TRUE) {
    .warnIfIndexingUnreorderedImage(x)
    return (x[i$getNonzeroIndices(array=TRUE,...), drop=drop])
})

#' @rdname index
#' @export
setReplaceMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - 1
    if (nArgs < 2)
        x$data[] <- value
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        x$data[indices] <- value
    }
    else
        x$data[,,...] <- value
    x$setSource(NULL)
    return (x)
})

#' @rdname index
#' @export
setReplaceMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    nArgs <- nargs() - 1
    if (nArgs < 3)
        x$data[i] <- value
    else if (x$isSparse())
    {
        indices <- .evaluateIndices(i, NULL, ...)
        x$data[indices] <- value
    }
    else
        x$data[i,,...] <- value
    x$setSource(NULL)
    return (x)
})

#' @rdname index
#' @export
setReplaceMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(NULL, j, ...)
        x$data[indices] <- value
    }
    else
        x$data[,j,...] <- value
    x$setSource(NULL)
    return (x)
})

#' @rdname index
#' @export
setReplaceMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    if (x$isSparse())
    {
        indices <- .evaluateIndices(i, j, ...)
        x$data[indices] <- value
    }
    else
        x$data[i,j,...] <- value
    x$setSource(NULL)
    return (x)
})

#' @rdname index
#' @export
setReplaceMethod("[", signature(x="MriImage",i="MriImage",j="missing"), function (x, i, j, ..., value) {
    .warnIfIndexingUnreorderedImage(x)
    x$data[i$getNonzeroIndices(array=TRUE,...)] <- value
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
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
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
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @export
mergeMriImages <- function (..., bindDim = NULL, padTags = FALSE)
{
    images <- lapply(list(...), as, "MriImage")
    if (length(images) == 1)
        return (images[[1]])
    if (!allEqual(sapply(images, orientation)))
        images <- lapply(images, reorderMriImage)
    if (!allEqual(lapply(images, xform), tolerance=1e-4))
        report(OL$Warning, "Merging images with nonequal xforms - this is probably unwise")
    
    dimensionalities <- sapply(images, fx(x$getDimensionality()))
    lastDim <- max(dimensionalities, bindDim)
    dimensions <- sapply(seq_along(images), fi(c(images[[i]]$getDimensions(), rep(1L,lastDim-dimensionalities[i]))))
    
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
    
    tags <- lapply(seq_along(images), fi(c(list(.blocks=blockCounts[i]), images[[i]]$getTags())))
    tagNames <- Reduce(intersect, lapply(tags, names))
    tags <- Reduce(function(x,y) {
        sapply(tagNames, function(n) {
            if (n == ".blocks")
                x[[n]] + y[[n]]
            else if (equivalent(x[[n]], y[[n]]))
                x[[n]]
            else if (length(x[[n]]) == x$.blocks && length(y[[n]]) == y$.blocks)
                c(x[[n]], y[[n]])
            else if (nrow(promote(x[[n]],TRUE)) == x$.blocks && nrow(promote(y[[n]],TRUE)) == y$.blocks)
                rbind(x[[n]], y[[n]])
            else if (padTags && length(x[[n]]) == x$.blocks)
                c(x[[n]], rep(NA,y$.blocks))
            else if (padTags && length(y[[n]]) == y$.blocks)
                c(rep(NA,x$.blocks), y[[n]])
            else
                NULL
        }, simplify=FALSE, USE.NAMES=TRUE)
    }, tags)
    
    tags$.blocks <- NULL
    
    return (asMriImage(data, images[[which.max(imageSizes)]], tags=tags[!sapply(tags,is.null)]))
}
