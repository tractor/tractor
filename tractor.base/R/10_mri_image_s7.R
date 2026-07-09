#' The mriImage class (S7)
#'
#' This S7 class represents an MRI image, made up of some voxel data (stored
#' as a sparse or dense numeric array) and some metadata, such as the file it
#' was read from, the voxel dimensions, and so on. This is a value-semantics
#' replacement for the \code{\linkS4class{MriImage}} reference class, which
#' is now a thin backwards-compatible shell around this class.
#'
#' @param imageDims Integer vector of dimensions.
#' @param voxelDims Numeric vector of pixel/voxel spacings.
#' @param voxelDimUnits Character vector of spatial and/or temporal spacing
#'   units. Millimetres and seconds (i.e., c("mm","s")) are typical.
#' @param source String naming the file(s) that the image was read from.
#' @param origin Numeric vector giving the spatial coordinate origin.
#' @param xform Numeric matrix giving the NIfTI-style xform matrix.
#' @param reordered Logical value indicating whether the image has been
#'   reordered.
#' @param tags Named list of arbitrary DICOM-style tags.
#' @param data Sparse or dense array of data, or \code{NULL}.
#'
#' @export
mriImage <- S7::new_class("mriImage", package = NULL, properties = list(
    imageDims = S7::class_integer,
    voxelDims = S7::class_numeric,
    voxelDimUnits = S7::class_character,
    source = S7::class_character,
    origin = S7::class_numeric,
    xform = S7::new_S3_class("matrix"),
    reordered = S7::class_logical,
    tags = S7::class_list,
    data = S7::new_union(sparseArray, S7::new_S3_class("rgbArray"), S7::new_S3_class("array"), NULL)),
    validator = function(self) {
        # NB: an empty imageDims (length 0) is a valid transient "uninitialised"
        # state - it arises e.g. when the legacy RefClass shell's built-in
        # $copy() calls new() bare (triggering initialize() with all-NULL
        # defaults) before overwriting fields, mirroring the original class's
        # own leniency (it never validated eagerly either)
        if (length(self@imageDims) > 0 && length(self@imageDims) < 2)
            return ("Image must be multidimensional")
        if (length(self@imageDims) != length(self@voxelDims))
            return ("Image and voxel dimensions should have the same length")
        NULL
    })

S7::S4_register(mriImage)

#' Create an mriImage object
#'
#' Constructor helper used by both the S7 constructor and the legacy
#' \code{\linkS4class{MriImage}} shell. Performs the same defaulting and
#' backwards-compatibility fix-ups as the original reference class's
#' \code{initialize} method.
#'
#' @param imageDims,voxelDims,voxelDimUnits,source,origin,xform,reordered,tags,data
#'   As for the \code{\link{mriImage}} class.
#' @param ... Additional legacy field names, for backwards compatibility
#'   (\code{imagedims}, \code{voxdims}, \code{voxunit}, \code{storedXform}).
#' @return An \code{mriImage} object.
#' @export
newMriImage <- function (imageDims = NULL, voxelDims = NULL, voxelDimUnits = NULL, source = "", origin = NULL, xform = emptyMatrix(), reordered = TRUE, tags = list(), data = NULL, ...)
{
    # Accept a legacy SparseArray shell transparently, unwrapping it to the
    # underlying sparseArray S7 value which is what the mriImage class
    # actually requires for its "data" property
    if (inherits(data, "SparseArray"))
        data <- data$value.

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

    if (!is.null(data))
        dim(data) <- .dims

    # Various fix-ups for backwards compatibility
    if (length(.voxunits) == 0)
        .voxunits <- "unknown"
    source <- as.character(source)
    if (length(source) != 1 || source == "internal")
        source <- ""
    if (length(tags) == 2 && all(c("keys","values") %in% names(tags)))
        tags <- structure(as.list(tags$values), names=tags$keys)
    origin <- as.numeric(origin)
    if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    else
        origin <- origin[1:3]

    mriImage(imageDims=as.integer(.dims), voxelDims=abs(as.numeric(.voxdims)), voxelDimUnits=.voxunits, source=source, origin=origin, xform=as.matrix(.xform), reordered=reordered, tags=tags, data=data)
}

#' @export
print.mriImage <- function (x, ...)
{
    summary <- .mriImageSummarise(x)
    printLabelledValues(summary$labels, summary$values)
    invisible(x)
}

## dim/as.array/Math/Ops/Summary/[/[<- are base R "internal generic" primitives.
## As discovered while porting sparseArray, registering an S7 method for one of
## these breaks plain S3 dispatch for *other* classes package-wide. They are
## implemented here as ordinary S3 methods instead - never via method().

#' @export
dim.mriImage <- function (x) x@imageDims

#' @export
as.array.mriImage <- function (x, ...)
{
    if (is.null(x@data))
        return (NULL)
    else if (is(x@data, "sparseArray"))
        return (as.array(x@data))
    else
        return (as.array(x@data))
}

#' @export
Math.mriImage <- function (x, ...)
{
    imageMap(x, .Generic)
}

#' @export
Ops.mriImage <- function (e1, e2)
{
    imageMap(e1, .Generic, e2)
}

#' @export
Summary.mriImage <- function (x, ..., na.rm = FALSE)
{
    if (nargs() > 2)
        report(OL$Error, "Function \"#{.Generic}\" is not defined for more than one image object")
    get(.Generic)(values(x), na.rm=na.rm)
}

.evaluateIndicesMriImage <- function (i, j, ...)
{
    args <- substitute(list(i, j, ...), parent.frame())
    argsEmpty <- sapply(args, function(a) identical(as.character(a), ""))
    args[argsEmpty] <- list(NULL)
    return (eval(args, envir=parent.frame(2)))
}

.warnIfIndexingUnreorderedMriImage <- function (image)
{
    if (!isTRUE(image@reordered) && orientation(image) != "LAS")
        flag(OL$Warning, "Indexing into an image which is not reordered has no consistent meaning")
}

#' @export
"[.mriImage" <- function (x, i, j, ..., drop = TRUE)
{
    .warnIfIndexingUnreorderedMriImage(x)
    nArgs <- nargs() - as.integer(!missing(drop))
    isSparse <- is(x@data, "sparseArray")

    if (missing(i) && missing(j))
    {
        if (nArgs < 2)
            return (x@data)
        else if (isSparse)
            return (x@data[.evaluateIndicesMriImage(NULL, NULL, ...),drop=drop])
        else
            return (x@data[,,...,drop=drop])
    }
    else if (!missing(i) && missing(j))
    {
        if (inherits(i, "mriImage"))
            return (x[.mriImageGetNonzeroIndices(i, array=TRUE, ...), drop=drop])
        else if (nArgs < 3)
            return (x@data[i,drop=drop])
        else if (isSparse)
            return (x@data[.evaluateIndicesMriImage(i, NULL, ...),drop=drop])
        else
            return (x@data[i,,...,drop=drop])
    }
    else if (missing(i) && !missing(j))
    {
        if (isSparse)
            return (x@data[.evaluateIndicesMriImage(NULL, j, ...),drop=drop])
        else
            return (x@data[,j,...,drop=drop])
    }
    else
    {
        if (isSparse)
            return (x@data[.evaluateIndicesMriImage(i, j, ...),drop=drop])
        else
            return (x@data[i,j,...,drop=drop])
    }
}

#' @export
"[<-.mriImage" <- function (x, i, j, ..., value)
{
    .warnIfIndexingUnreorderedMriImage(x)
    nArgs <- nargs() - 1
    isSparse <- is(x@data, "sparseArray")
    newData <- x@data

    if (missing(i) && missing(j))
    {
        if (nArgs < 2)
            newData[] <- value
        else if (isSparse)
            newData[.evaluateIndicesMriImage(i, j, ...)] <- value
        else
            newData[,,...] <- value
    }
    else if (!missing(i) && missing(j) && inherits(i, "mriImage"))
        newData[.mriImageGetNonzeroIndices(i, array=TRUE)] <- value
    else if (!missing(i) && missing(j))
    {
        if (nArgs < 3)
            newData[i] <- value
        else if (isSparse)
            newData[.evaluateIndicesMriImage(i, NULL, ...)] <- value
        else
            newData[i,,...] <- value
    }
    else if (missing(i) && !missing(j))
    {
        if (isSparse)
            newData[.evaluateIndicesMriImage(NULL, j, ...)] <- value
        else
            newData[,j,...] <- value
    }
    else
    {
        if (isSparse)
            newData[.evaluateIndicesMriImage(i, j, ...)] <- value
        else
            newData[i,j,...] <- value
    }

    set_props(x, data=newData, source="")
}

# RNifti generic accessors: reuse pixdim/pixunits (already S3 generics from
# RNifti), rather than tractor.base's old getVoxelDimensions()/getVoxelUnits()
# names, per the project's dependency-naming convention.

#' @export
pixdim.mriImage <- function (object) object@voxelDims

#' @export
"pixdim<-.mriImage" <- function (object, value)
{
    set_props(object, voxelDims=abs(as.numeric(value)), source="")
}

#' @export
pixunits.mriImage <- function (object) object@voxelDimUnits

#' @export
"pixunits<-.mriImage" <- function (object, value)
{
    set_props(object, voxelDimUnits=as.character(value), source="")
}

#' @export
method(values, mriImage) <- function (x) x@data

#' @export
method(`values<-`, mriImage) <- function (x, value)
{
    if (inherits(value, "SparseArray"))
        value <- value$value.
    set_props(x, data=value, source="")
}

#' A metadata-only view of an image
#'
#' Returns a version of an \code{\link{mriImage}} with any voxel data
#' removed, retaining only its metadata.
#'
#' @param x An \code{mriImage} object.
#' @export
metadata <- S7::new_generic("metadata", "x")

#' @export
method(metadata, mriImage) <- function (x)
{
    set_props(x, data=NULL)
}

#' Replace an image's voxel values with the result of a function
#'
#' Applies a function to the voxel data of one or more images (and/or plain
#' arrays), and returns a new \code{\link{mriImage}} with the result. Any
#' \code{mriImage} arguments beyond the first are converted to plain arrays
#' before the function is applied.
#'
#' @param x An \code{mriImage} object.
#' @param fun A function to apply.
#' @param ... Additional arguments to \code{fun}, which may include further
#'   \code{mriImage} objects.
#' @param sparse Logical value: should the result be stored sparsely? The
#'   default, \code{NULL}, chooses automatically based on the proportion of
#'   zeroes in the result.
#' @export
imageMap <- S7::new_generic("imageMap", "x")

#' @export
method(imageMap, mriImage) <- function (x, fun, ..., sparse = NULL)
{
    args <- lapply(c(list(x),list(...)), function (a) {
        if (inherits(a, "mriImage"))
            return (as.array(a))
        else
            return (a)
    })
    result <- do.call(fun, args)

    if (!equivalent(dim(result), x@imageDims))
        dim(result) <- x@imageDims

    if (is.null(sparse))
        sparse <- (sum(result==0) / length(result) >= 0.75)
    if (isTRUE(sparse))
        result <- asSparseArray(result)

    set_props(x, data=result, source="")
}

#' Threshold an image
#'
#' @param x An \code{mriImage} object.
#' @param level The threshold level.
#' @param defaultValue The value to assign to voxels below the threshold.
#' @export
threshold <- S7::new_generic("threshold", "x")

#' @export
method(threshold, mriImage) <- function (x, level, defaultValue = 0)
{
    imageMap(x, function(v) ifelse(v >= level, v, defaultValue))
}

#' Binarise an image
#'
#' @param x An \code{mriImage} object.
#' @export
binarise <- S7::new_generic("binarise", "x")

#' @export
method(binarise, mriImage) <- function (x)
{
    imageMap(x, function(v) ifelse(v!=0, 1L, 0L))
}

#' Mask an image
#'
#' @param x An \code{mriImage} object.
#' @param maskImage Another \code{mriImage}, defining the mask.
#' @export
mask <- S7::new_generic("mask", "x")

#' @export
method(mask, mriImage) <- function (x, maskImage)
{
    imageMap(x, function(v,m) ifelse(m==0,0,v), maskImage)
}

## The remaining query-style accessors below are implemented as simple plain
## functions rather than new S7 generics, since they are specific to
## mriImage and have no cross-class reuse rationale. The legacy shell
## forwards to these under their original method names.

.mriImageIsEmpty <- function (x) is.null(x@data)
.mriImageIsSparse <- function (x) is(x@data, "sparseArray")
.mriImageIsRgb <- function (x) inherits(x@data, "rgbArray")
.mriImageIsReordered <- function (x) isTRUE(x@reordered)
.mriImageIsInternal <- function (x) (x@source == "")
.mriImageDimensionality <- function (x) length(x@voxelDims)
.mriImageFieldOfView <- function (x) abs(x@voxelDims) * x@imageDims
.mriImageNChannels <- function (x) ifelse(.mriImageIsRgb(x), attr(x@data,"channels") %||% 3L, 1L)
.mriImageNSlices <- function (x) ifelse(length(x@imageDims) > 2, x@imageDims[3], 1L)
.mriImageNTags <- function (x) length(x@tags)
.mriImageNVolumes <- function (x) ifelse(length(x@imageDims) > 3, prod(x@imageDims[-(1:3)]), 1L)
.mriImageHasTags <- function (x, keys) sapply(keys, function(key) !is.null(x@tags[[key]]))
.mriImageGetTags <- function (x, keys = NULL) indexList(x@tags, keys)

.mriImageSetTags <- function (x, ..., merge = FALSE)
{
    newTags <- deduplicate(list(...), x@tags, merge=merge)
    set_props(x, tags=newTags[!sapply(newTags,is.null)], source="")
}

.mriImageGetXform <- function (x, implicit = TRUE)
{
    if (is.emptyMatrix(x@xform) && .mriImageIsReordered(x) && implicit)
    {
        implicitXform <- diag(4)
        zeroBasedOrigin <- pmax(x@origin-1, c(0,0,0))
        if (.mriImageDimensionality(x) == 2)
        {
            implicitXform[c(1,6)] <- c(-1,1) * abs(x@voxelDims)
            zeroBasedOrigin[1:2] <- zeroBasedOrigin[1:2] * abs(x@voxelDims)
        }
        else
        {
            implicitXform[c(1,6,11)] <- c(-1,1,1) * abs(x@voxelDims[1:3])
            zeroBasedOrigin <- zeroBasedOrigin * abs(x@voxelDims[1:3])
        }
        implicitXform[1:3,4] <- c(1,-1,-1) * zeroBasedOrigin
        return (implicitXform)
    }
    else
        return (x@xform)
}

.mriImageGetDataAtPoint <- function (x, ...)
{
    if (is.null(x@data))
        return (NA)

    .warnIfIndexingUnreorderedMriImage(x)

    nd <- .mriImageDimensionality(x)
    loc <- resolveVector(len=nd, ...)
    if (is.null(loc) || (length(loc) != nd))
        report(OL$Error, "Point must be specified as a ", nd, "-vector")

    if (all(loc >= 1) && all(loc <= x@imageDims))
        return (x@data[matrix(loc,nrow=1)])
    else
        return (NA)
}

.mriImageGetSlice <- function (x, dim, loc)
{
    if (length(x@imageDims) < max(2,dim))
        report(OL$Error, "The dimensionality of the image is too low")
    if (!(loc %in% seq_len(x@imageDims[dim])))
        report(OL$Error, "The specified location is out of bounds")

    .warnIfIndexingUnreorderedMriImage(x)

    dimsToKeep <- setdiff(seq_along(x@imageDims), dim)
    if (.mriImageIsEmpty(x))
        newData <- NULL
    else if (.mriImageIsSparse(x))
    {
        newData <- array(0, dim=x@imageDims[dimsToKeep])
        matchingCoords <- which(coordinates(x@data)[,dim] == loc)
        newData[coordinates(x@data)[matchingCoords,dimsToKeep,drop=FALSE]] <- values(x@data)[matchingCoords]
    }
    else
    {
        indices <- alist(i=,j=,k=,t=,u=,v=,w=)[seq_along(x@imageDims)]
        indices[dim] <- loc
        newData <- do.call("[", c(list(x@data),indices))
        if (is.vector(newData))
            newData <- promote(newData)
    }

    invisible(newData)
}

.mriImageFind <- function (x, fun = NULL, ..., array = TRUE)
{
    .warnIfIndexingUnreorderedMriImage(x)

    if (.mriImageIsEmpty(x))
        return (integer(0))
    else if (is.null(fun))
    {
        if (.mriImageIsSparse(x))
        {
            locs <- coordinates(x@data)
            if (array)
                return (locs)
            else
                return (matrixToVectorLocs(locs, x@imageDims))
        }
        else
            return (which(x@data != 0, arr.ind=array))
    }
    else
    {
        if (is.numeric(fun) && length(fun) == 1)
            locs <- which(as.array(x@data) == fun)
        else
        {
            fun <- match.fun(fun)
            locs <- which(as.logical(fun(as.array(x@data), ...)))
        }

        if (array)
            return (vectorToMatrixLocs(locs, x@imageDims))
        else
            return (locs)
    }
}

.mriImageGetNonzeroIndices <- function (x, array = TRUE, positiveOnly = FALSE)
{
    .warnIfIndexingUnreorderedMriImage(x)
    if (positiveOnly)
        return (.mriImageFind(x, fx(x > 0), array=array))
    else
        return (.mriImageFind(x, array=array))
}

.mriImageGetSparseness <- function (x)
{
    if (.mriImageIsEmpty(x))
        return (NA)
    else if (.mriImageIsSparse(x))
        return (1 - (nrow(coordinates(x@data)) / prod(x@imageDims)))
    else
        return (sum(x@data==0 | is.na(x@data)) / prod(x@imageDims))
}

.mriImageFill <- function (x, value)
{
    if (value == 0)
        newData <- newSparseArray(data=vector(class(value),0), coords=matrix(NA,0,length(x@imageDims)), dims=x@imageDims)
    else
        newData <- array(value, dim=x@imageDims)
    set_props(x, data=newData, source="")
}

.mriImageApply <- function (x, ...)
{
    if (.mriImageIsEmpty(x))
        report(OL$Error, "The image contains no data")
    else if (.mriImageIsSparse(x))
        return (sparseApply(x@data, ...))
    else
        return (base::apply(x@data, ...))
}

.mriImageSummarise <- function (x)
{
    voxelDimUnits <- x@voxelDimUnits
    voxelDims <- x@voxelDims
    spatialUnit <- voxelDimUnits["spatial"]
    temporalUnit <- voxelDimUnits["temporal"]
    voxelDimString <- paste(implode(round(abs(voxelDims[1:min(3,length(voxelDims))]),5), sep=" x "), ifelse(!is.na(spatialUnit),paste(" ",spatialUnit,sep=""),""), sep="")
    if (length(voxelDims) > 3)
        voxelDimString <- paste(voxelDimString, " x ", round(abs(voxelDims[4]),5), ifelse(!is.na(spatialUnit) && !is.na(temporalUnit),paste(" ", temporalUnit,sep=""),""), sep="")
    if (length(voxelDims) > 4)
        voxelDimString <- paste(voxelDimString, " x ", implode(round(abs(voxelDims[5:length(voxelDims)]),5), sep=" x "), sep="")
    if (all(voxelDimUnits == "unknown"))
        voxelDimString <- paste(voxelDimString, "(units unknown)", sep=" ")

    tagNames <- names(x@tags)
    if (length(tagNames) == 0)
        tagNames <- "(none)"

    labels <- c("Image source", "Image dimensions", "Voxel dimensions", "Coordinate origin", "Additional tags")
    values <- c(ifelse(x@source=="","internal",x@source), paste(implode(x@imageDims, sep=" x "),"voxels",sep=" "), voxelDimString, paste("(",implode(round(x@origin,2), sep=","),")",sep=""), implode(tagNames,sep=", "))

    if (!.mriImageIsEmpty(x))
    {
        sparseness <- paste(round(.mriImageGetSparseness(x)*100,2), "% (", ifelse(.mriImageIsSparse(x),"sparse","dense"), " storage)", sep="")
        labels <- c(labels, "Sparseness")
        values <- c(values, sparseness)
    }

    return (list(labels=labels, values=values))
}
