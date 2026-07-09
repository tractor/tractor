#' The sparseArray class (S7)
#'
#' This S7 class represents an array with any number of dimensions, in which
#' a significant proportion of entries are zero. The coordinates of nonzero
#' entries are stored along with their values, with all remaining entries
#' assumed to be zero. This is a value-semantics replacement for the
#' \code{\linkS4class{SparseArray}} reference class, which is now a thin
#' backwards-compatible shell around this class.
#'
#' @param data Vector of nonzero data values.
#' @param coords Integer matrix of nonzero \code{data} locations, one per row.
#' @param dims Integer vector of dimensions.
#'
#' @export
sparseArray <- S7::new_class("sparseArray", package = NULL, properties = list(
    data = S7::class_any,
    coords = S7::new_S3_class("matrix"),
    dims = S7::class_integer),
    validator = function(self) {
        if (is.vector(self@data) && length(self@data) != nrow(self@coords))
            return ("data length and number of coordinate rows must agree")
        if (nrow(self@coords) > 0 && ncol(self@coords) != length(self@dims))
            return ("coords must have one column per array dimension")
        NULL
    })

# Register as a formal S4 class so that the legacy SparseArray shell can type
# its hidden field precisely, and so setClassUnion()/is() recognise it.
S7::S4_register(sparseArray)

#' Create a sparseArray object
#'
#' Constructor helper used by both the S7 constructor and the legacy
#' \code{\linkS4class{SparseArray}} shell. Performs the same coercions as the
#' original reference class's \code{initialize} method.
#'
#' @param data A vector of (nonzero) array elements.
#' @param coords A matrix with as many rows as \code{data} has elements.
#' @param dims The dimensions of the array.
#' @return A \code{sparseArray} object.
#' @export
newSparseArray <- function (data, coords, dims)
{
    storage.mode(coords) <- "integer"
    sparseArray(data=data, coords=coords, dims=as.integer(dims))
}

## NB: print/dim/dim<-/aperm/[/[<- are all base R "internal generic" primitives.
## Registering an S7 method for one of these via S7::method() was found (during
## implementation) to break plain S3 dispatch for *other*, unrelated classes in
## the same package (e.g. dim.MriImage stopped working once S7::method(dim,
## sparseArray) was registered) - apparently S7 converts the primitive into a
## formal generic package-wide as a side effect of registration, which doesn't
## reliably fall back to plain UseMethod dispatch for classes it doesn't know
## about. To avoid this, these are implemented as ordinary S3 methods below,
## exactly as the legacy classes already do - S3 dispatch on S7 objects works
## natively (verified), so no S7-specific mechanism is needed for these.

#' @export
print.sparseArray <- function (x, ...)
{
    printLabelledValues(c("Dimensions","Nonzero elements","Sparseness"),
        c(implode(x@dims,sep=" x "), as.character(length(x@data)), paste0(round(100*(1-length(x@data)/prod(x@dims)),2),"%")))
    invisible(x)
}

#' @rdname accessors
#' @export
coordinates <- S7::new_generic("coordinates", "x")

#' @export
method(coordinates, sparseArray) <- function (x) x@coords

#' Value and coordinate accessors
#'
#' @param x An object.
#' @param value Replacement value.
#' @name accessors
#' @rdname accessors
#' @export
values <- S7::new_generic("values", "x")

#' @rdname accessors
#' @export
"values<-" <- S7::new_generic("values<-", "x")

#' @export
method(values, sparseArray) <- function (x) x@data

#' @export
method(`values<-`, sparseArray) <- function (x, value)
{
    if (length(value) != nrow(x@coords))
        report(OL$Error, "Coordinate matrix and data vector lengths don't agree")
    prop(x, "data") <- value
    x
}

## Dimensionality is handled by RNifti::ndim(), a plain function equal to
## length(dim(object)) - no method is needed here since dim.sparseArray
## (below) already lets it work correctly.

#' Sparse-aware margin-wise function application
#'
#' Applies a function to margins of a \code{\link{sparseArray}} without
#' first expanding it to a dense array.
#'
#' @param x A \code{sparseArray} object.
#' @param margin The dimension(s) to keep, passed to the function.
#' @param fun A function to apply.
#' @param ... Additional arguments to \code{fun}.
#' @export
sparseApply <- S7::new_generic("sparseApply", "x")

#' @export
method(sparseApply, sparseArray) <- function (x, margin, fun, ...)
{
    fun <- match.fun(fun)
    dimsToKeep <- x@dims[margin]
    dimsToLose <- x@dims[-margin]
    iterations <- prod(dimsToKeep)

    reshaped <- aperm(x, c(setdiff(seq_len(ndim(x)),margin), margin))
    reshaped <- `dim<-`(reshaped, c(prod(dimsToLose), iterations))

    tempData <- as.array(reshaped[,1])
    dim(tempData) <- dimsToLose
    tempResult <- fun(tempData, ...)
    finalArray <- array(NA, dim=c(length(tempResult),iterations))
    for (i in 1:iterations)
    {
        tempData <- as.array(reshaped[,i])
        dim(tempData) <- dimsToLose
        finalArray[,i] <- fun(tempData, ...)
    }

    dim(finalArray) <- c(dim(finalArray)[1], dimsToKeep)
    finalArray <- drop(finalArray)
    if (length(dim(finalArray)) == 1)
        dim(finalArray) <- NULL

    finalArray
}

#' Flip a sparse array along one or more dimensions
#' @export
flip <- S7::new_generic("flip", "x")

#' @export
method(flip, sparseArray) <- function (x, dimsToFlip)
{
    newCoords <- x@coords
    for (i in dimsToFlip)
        newCoords[,i] <- x@dims[i] - newCoords[,i] + 1
    prop(x, "coords") <- newCoords
    x
}

#' @export
dim.sparseArray <- function (x) x@dims

#' @export
"dim<-.sparseArray" <- function (x, value)
{
    if (prod(x@dims) != prod(value))
        report(OL$Error, "New dimensions are incompatible with this sparseArray object")
    newCoords <- vectorToMatrixLocs(matrixToVectorLocs(x@coords,x@dims), value)
    set_props(x, coords=newCoords, dims=as.integer(value))
}

#' @export
aperm.sparseArray <- function (a, perm, ...)
{
    set_props(a, coords=a@coords[,perm,drop=FALSE], dims=a@dims[perm])
}

.evaluateIndicesS7 <- function (i, j, ...)
{
    args <- substitute(list(i, j, ...), parent.frame())
    argsEmpty <- sapply(args, function(a) identical(as.character(a), ""))
    args[argsEmpty] <- list(NULL)
    return (eval(args, envir=parent.frame(2)))
}

#' @export
"[.sparseArray" <- function (x, i, j, ..., drop = TRUE)
{
    nArgs <- nargs() - as.integer(!missing(drop))

    nDims <- ndim(x)
    dims <- x@dims
    data <- x@data
    coords <- x@coords

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
                if (ncol(index) != nDims)
                    report(OL$Error, "Number of dimensions given does not match image")
                index <- matrixToVectorLocs(index, dims)
            }

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
        args <- .evaluateIndicesS7(i, j, ...)

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
        returnValue <- vector(mode=storage.mode(data), length=prod(finalDims))
        returnValue[apply(finalCoords[dataToKeep,,drop=FALSE],1,max)] <- data[dataToKeep]
    }
    else
        returnValue <- newSparseArray(data=data[dataToKeep], coords=finalCoords[dataToKeep,dimsToKeep,drop=FALSE], dims=finalDims[dimsToKeep])

    return (returnValue)
}

#' @export
"[<-.sparseArray" <- function (x, i, j, ..., value)
{
    nArgs <- nargs() - 1

    nDims <- ndim(x)
    dims <- x@dims
    data <- x@data
    coords <- x@coords

    if (nArgs < 2)
        index <- matrixToVectorLocs(coords, dims)
    else if (nArgs == 2 && !is.list(i))
    {
        index <- i
        if (is.logical(index) || is.character(index))
            report(OL$Error, "Logical and character indexing are not yet supported")
        else if (is.matrix(index))
        {
            if (ncol(index) != nDims)
                report(OL$Error, "Number of dimensions given does not match image")
            index <- matrixToVectorLocs(index, dims)
        }

        if (any(index <= 0))
            report(OL$Error, "Zero and negative indices are not yet supported")
    }
    else if (nArgs != (nDims + 1) && !is.list(i))
        report(OL$Error, "Number of dimensions given does not match image")
    else
    {
        args <- .evaluateIndicesS7(i, j, ...)
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

    set_props(x, coords=coords, data=data)
}

#' @export
Ops.sparseArray <- function (e1, e2)
{
    generic <- .Generic
    isSparse1 <- inherits(e1, "sparseArray")
    x <- if (isSparse1) e1 else e2
    other <- if (isSparse1) e2 else e1

    if (missing(other) || is.null(other))
        report(OL$Error, "No operator method is defined for a \"sparseArray\" and a missing operand")
    else if (is.array(other) || is.matrix(other))
    {
        if (!equivalent(x@dims, dim(other)))
            report(OL$Error, "Sparse and dense array dimensions don't match")
        if (!all(get(generic)(0, unique(other)) == 0))
            return (if (isSparse1) get(generic)(as.array(x), other) else get(generic)(other, as.array(x)))
        else
        {
            newData <- if (isSparse1) get(generic)(x@data, other[x@coords]) else get(generic)(other[x@coords], x@data)
            zero <- (newData == 0)
            return (newSparseArray(data=newData[!zero], coords=x@coords[!zero,,drop=FALSE], dims=x@dims))
        }
    }
    else if (is.numeric(other) || is.logical(other))
    {
        if (get(generic)(0, other[1]) != 0)
            return (if (isSparse1) get(generic)(as.array(x), other) else get(generic)(other, as.array(x)))
        else
        {
            newData <- if (isSparse1) get(generic)(x@data, other) else get(generic)(other, x@data)
            zero <- (newData == 0)
            return (newSparseArray(data=newData[!zero], coords=x@coords[!zero,,drop=FALSE], dims=x@dims))
        }
    }
    else
        report(OL$Error, "No operator method is defined for a \"sparseArray\" and \"#{class(other)[1]}\"")
}

#' @export
Summary.sparseArray <- function (x, ..., na.rm = FALSE)
{
    get(.Generic)(0, x@data, ..., na.rm=na.rm)
}

#' @export
as.array.sparseArray <- function (x, ...)
{
    result <- array(vector(mode=storage.mode(x@data),length=1), dim=x@dims)
    result[x@coords] <- x@data
    result
}

#' @export
as.vector.sparseArray <- function (x, mode = "any")
{
    as.vector(as.array(x), mode=mode)
}

#' Convert a sparse array to a logical array
#' @export
asLogicalArray <- function (x)
{
    result <- array(FALSE, dim=x@dims)
    result[x@coords] <- TRUE
    as.vector(result)
}

#' Create a sparseArray from a dense array
#' @export
asSparseArray <- function (x)
{
    coordinates <- which(!is.na(x) & x != 0, arr.ind=TRUE)
    newSparseArray(data=x[coordinates], coords=coordinates, dims=dim(x))
}
