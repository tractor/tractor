#' The SparseArray class
#' 
#' This class represents an array with any number of dimensions, in which a
#' significant proportion of entries are zero. The coordinates of nonzero
#' entries are stored along with their values, with all remaining entries
#' assumed to be zero. Methods are provided to index into the array in the
#' standard way, using matrix or vector indices; and for coercing between
#' \code{SparseArray} objects and standard (dense) arrays.
#' 
#' This is a thin backwards-compatible shell around the value-semantics
#' \code{\link{sparseArray}} S7 class, which holds the actual data and
#' implements all of the real logic. Every method here forwards to it.
#'
#' @field data Vector of nonzero data values
#' @field coords Integer matrix of nonzero \code{data} locations, one per row
#' @field dims Integer vector of dimensions
#' 
#' @export
SparseArray <- setRefClass("SparseArray", contains="SerialisableObject", fields=list(value.="sparseArray"), methods=list(
    initialize = function (data = NULL, coords = emptyMatrix(), dims = integer(0), value. = NULL, ...)
    {
        if (!is.null(value.))
            object <- initFields(value.=value.)
        else
            object <- initFields(value.=newSparseArray(data=data, coords=coords, dims=dims))
        return (object)
    },
    
    aperm = function (perm)
    {
        "Permute the dimensions of the array"
        .self$value. <- aperm(value., perm)
        invisible (.self)
    },
    
    apply = function (margin, fun, ...)
    {
        "Apply a function to margins of the array"
        sparseApply(value., margin, fun, ...)
    },
    
    flip = function (dimsToFlip)
    {
        "Flip the array along one or more directions"
        .self$value. <- flip(value., dimsToFlip)
        invisible (.self)
    },
    
    getCoordinates = function () { return (coordinates(value.)) },
    
    getData = function () { return (values(value.)) },
    
    getDimensionality = function () { return (ndim(value.)) },
    
    getDimensions = function () { return (dim(value.)) },
    
    setCoordinatesAndData = function (newCoords, newData)
    {
        "Update the nonzero locations and data values in the array"
        if (length(newData) != nrow(newCoords))
            report(OL$Error, "Coordinate matrix and data vector lengths don't agree")
        .self$value. <- set_props(value., coords=newCoords, data=newData)
        invisible (.self)
    },
    
    setDimensions = function (newDims)
    {
        "Change the dimensions of the image"
        dim(.self$value.) <- newDims
        invisible (.self)
    },

    serialise = function (file = NULL)
    {
        "Serialise the object to a list or file"
        fields <- list(data=values(value.), coords=coordinates(value.), dims=dim(value.))
        out <- structure(fields, originalClass="SparseArray", originalPackage="tractor.base")
        if (!is.null(file))
            save(out, file=ensureFileSuffix(file,"Rdata"))
        invisible (out)
    },
    
    summarise = function ()
    {
        labels <- c("Dimensions", "Nonzero elements", "Sparseness")
        vals <- c(implode(dim(value.),sep=" x "), as.character(length(values(value.))), paste0(round(100*(1-length(values(value.))/prod(dim(value.))),2),"%"))
        return (list(labels=labels, values=vals))
    }
))

.evaluateIndices <- function (i, j, ...)
{
    # Find missing arguments and replace with NULL (code borrowed from the "slam" package)
    args <- substitute(list(i, j, ...), parent.frame())
    argsEmpty <- sapply(args, function(a) identical(as.character(a), ""))
    args[argsEmpty] <- list(NULL)
    return (eval(args, envir=parent.frame(2)))
}

#' Indexing methods
#' 
#' Indexing methods for \code{\link{SparseArray}} and \code{\link{MriImage}}
#' objects. For the latter class, arguments are passed to the equivalents for
#' \code{array} or \code{\link{SparseArray}}, except where \code{i} is another
#' \code{\link{MriImage}} object, where its nonzero region will be used to
#' provide the indices. For \code{\link{SparseArray}}, indexing may be blank,
#' or by numeric vector or matrix.
#' 
#' @param x An object of the appropriate type.
#' @param i,j,\dots Indexing objects.
#' @param drop Scalar value: should unitary dimensions be dropped?
#' @param value New value(s) for replacement forms.
#' @return A vector, array or \code{\link{SparseArray}}.
#' 
#' @author Jon Clayden
#' @rdname index-legacy
#' @export
setMethod("[", "SparseArray", function (x, i, j, ..., drop = TRUE) {
    # This implementation owes a lot to the equivalent in the "slam" package (credit: Kurt Hornik, David Meyer and Christian Buchta)
    nArgs <- nargs() - as.integer(!missing(drop))
    
    nDims <- x$getDimensionality()
    dims <- x$getDimensions()
    data <- x$getData()
    coords <- x$getCoordinates()
    
    if (nArgs < 2)
        return (data)
    else if (nArgs == 2)
    {
        if (is.list(i))
            args <- i
        else
        {
            index <- i
            if (is.logical(index) || is.character(index))
                report(OL$Error, "Logical and character indexing are not yet supported")
            else if (is.matrix(index))
            {
                # Matrix indexing, one row per point: convert to vector and drop through
                if (ncol(index) != nDims)
                    report(OL$Error, "Number of dimensions given does not match image")
                index <- matrixToVectorLocs(index, dims)
            }
        
            # Vector indexing, one number per point
            if (any(index <= 0))
                report(OL$Error, "Zero and negative indices are not yet supported")
        
            returnValue <- vector(mode=storage.mode(data), length=length(index))
            dataLocs <- match(index, matrixToVectorLocs(coords,dims), 0L)
            returnValue[dataLocs > 0] <- data[dataLocs]
        
            return (returnValue)
        }
    }
    else if (nArgs != (nDims + 1))
        report(OL$Error, "Number of dimensions given does not match image")
    else
        args <- .evaluateIndices(i, j, ...)
    
    dataToKeep <- rep.int(TRUE, length(data))
    finalDims <- dims
    finalCoords <- coords
    
    for (currentDim in 1:nDims)
    {
        currentDimIndex <- args[[currentDim]]
        if (is.null(currentDimIndex))
            next
        else if (!is.numeric(currentDimIndex))
            report(OL$Error, "Only numeric indices are currently supported")
        else if (any(currentDimIndex <= 0))
            report(OL$Error, "Zero and negative indices are not yet supported")
        else
        {
            finalDims[currentDim] <- length(currentDimIndex)
            dataLocs <- match(coords[,currentDim], currentDimIndex, 0L)
            finalCoords[dataLocs > 0, currentDim] <- seq_along(currentDimIndex)[dataLocs]
            dataToKeep <- dataToKeep & (dataLocs > 0)
        }
    }
    
    if (drop)
        dimsToKeep <- which(finalDims > 1)
    else
        dimsToKeep <- 1:nDims
    
    if (length(dimsToKeep) < 2)
    {
        # Only one dimension is nonunitary, so the index into the final
        # vector is the maximum of the indices into each dimension for
        # each nonzero data value - the other indices will all be 1
        returnValue <- vector(mode=storage.mode(data), length=prod(finalDims))
        returnValue[apply(finalCoords[dataToKeep,,drop=FALSE],1,max)] <- data[dataToKeep]
    }
    else
        returnValue <- SparseArray$new(value.=newSparseArray(data=data[dataToKeep], coords=finalCoords[dataToKeep,dimsToKeep,drop=FALSE], dims=finalDims[dimsToKeep]))
    
    return (returnValue)
})

#' @rdname index-legacy
#' @export
setReplaceMethod("[", "SparseArray", function (x, i, j, ..., value) {
    nArgs <- nargs() - 1
    
    nDims <- x$getDimensionality()
    dims <- x$getDimensions()
    data <- x$getData()
    coords <- x$getCoordinates()
    
    if (nArgs < 2)
        index <- matrixToVectorLocs(coords, dims)
    else if (nArgs == 2 && !is.list(i))
    {
        index <- i
        if (is.logical(index) || is.character(index))
            report(OL$Error, "Logical and character indexing are not yet supported")
        else if (is.matrix(index))
        {
            # Matrix indexing, one row per point: convert to vector and drop through
            if (ncol(index) != nDims)
                report(OL$Error, "Number of dimensions given does not match image")
            index <- matrixToVectorLocs(index, dims)
        }
        
        # Vector indexing, one number per point
        if (any(index <= 0))
            report(OL$Error, "Zero and negative indices are not yet supported")
    }
    # Don't evaluate i unless necessary - it may be missing
    else if (nArgs != (nDims + 1) && !is.list(i))
        report(OL$Error, "Number of dimensions given does not match image")
    else
    {
        args <- .evaluateIndices(i, j, ...)
        if (is.list(args[[1]]))
            args <- args[[1]]
        
        args <- lapply(seq_len(nDims), function(i) {
            if (is.null(args[[i]]))
                1:dims[i]
            else
                args[[i]]
        })
        
        index <- as.matrix(expand.grid(args))
        storage.mode(index) <- "integer"
        index <- matrixToVectorLocs(index, dims)
    }
    
    if (length(index) %% length(value) != 0)
        report(OL$Error, "Number of items to replace is not a multiple of replacement length")
    if (length(index) != length(value))
        value <- rep(value, length(index) %/% length(value))
    
    dataLocs <- match(index, matrixToVectorLocs(coords,dims), 0L)
    present <- (dataLocs > 0)
    zero <- (!is.na(value) & value == 0)
    
    data[dataLocs[present & !zero]] <- value[which(present & !zero)]
    if (any(present & zero))
    {
        coords <- coords[-dataLocs[present & zero],,drop=FALSE]
        data <- data[-dataLocs[present & zero]]
    }
    if (any(!present & !zero))
    {
        coords <- rbind(coords, arrayInd(index[which(!present & !zero)],dims))
        data <- c(data, value[which(!present & !zero)])
    }
    
    x$setCoordinatesAndData(coords, data)
    return (x)
})

setMethod("Ops", signature(e1="SparseArray",e2="ANY"), function (e1, e2) {
    result <- callGeneric(e1$value., e2)
    if (is(result, "sparseArray"))
        return (SparseArray$new(value.=result))
    else
        return (result)
})

setMethod("Summary", signature(x="SparseArray"), function(x, ..., na.rm = FALSE) {
    callGeneric(x$value., ..., na.rm=na.rm)
})

setAs("array", "SparseArray", function (from) {
    return (SparseArray$new(value.=asSparseArray(from)))
})

setAs("SparseArray", "array", function (from) {
    return (as.array(from$value.))
})

setAs("SparseArray", "logical", function (from) {
    return (asLogicalArray(from$value.))
})

#' @export
as.array.SparseArray <- function (x, ...)
{
    as(x, "array")
}

#' @export
as.vector.SparseArray <- function (x, mode = "any")
{
    as.vector(as(x,"array"), mode=mode)
}

#' @export
dim.SparseArray <- function (x)
{
    x$getDimensions()
}

#' @export
"dim<-.SparseArray" <- function (x, value)
{
    x$setDimensions(value)
    return (x)
}

#' @export
aperm.SparseArray <- function (a, perm, ...)
{
    SparseArray$new(value.=aperm(a$value., perm))
}

#' Create a SparseArray object
#' 
#' This function creates a \code{\linkS4class{SparseArray}} object from its
#' constituent parts.
#' 
#' @param data A vector of (nonzero) array elements.
#' @param coordinates A matrix with as many rows as \code{data} has elements,
#'   containing the coordinates of each nonzero element in the array.
#' @param dims The dimensions of the array.
#' @return A \code{\linkS4class{SparseArray}} object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export newSparseArrayWithData
newSparseArrayWithData <- function (data, coordinates, dims)
{
    invisible (SparseArray$new(value.=newSparseArray(data=data, coords=coordinates, dims=dims)))
}
