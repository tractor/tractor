SparseArray <- setRefClass("SparseArray", contains="SerialisableObject", fields=list(data="ANY",coords="matrix",dims="integer"), methods=list(
    initialize = function (...)
    {
        object <- initFields(...)
        storage.mode(object$coords) <- "integer"
        return (object)
    },
    
    aperm = function (perm)
    {
        .self$coords <- .self$coords[,perm]
        .self$dims <- .self$dims[perm]
    },
    
    apply = function (margin, fun, ...)
    {
        fun <- match.fun(fun)
        dimsToKeep <- .self$dims[margin]
        dimsToLose <- .self$dims[-margin]
        iterations <- prod(dimsToKeep)
        
        reshapedArray <- .self$copy()
        reshapedArray$aperm(c(setdiff(seq_len(.self$getDimensionality()),margin), margin))
        reshapedArray$setDimensions(c(prod(dimsToLose), iterations))
        
        # Assume for now that each application of the function will result in a vector of the same length
        tempData <- as.array(reshapedArray[,1])
        dim(tempData) <- dimsToLose
        tempResult <- fun(tempData, ...)
        finalArray <- array(NA, dim=c(length(tempResult),iterations))
        for (i in 1:iterations)
        {
            tempData <- as.array(reshapedArray[,i])
            dim(tempData) <- dimsToLose
            finalArray[,i] <- fun(tempData, ...)
        }
        
        dim(finalArray) <- c(dim(finalArray)[1], dimsToKeep)
        finalArray <- drop(finalArray)
        if (length(dim(finalArray)) == 1)
            dim(finalArray) <- NULL
        
        return (finalArray)
    },
    
    flip = function (dimsToFlip)
    {
        for (i in dimsToFlip)
            .self$coords[,i] <- .self$dims[i] - .self$coords[,i] + 1
    },
    
    getCoordinates = function () { return (.self$coords) },
    
    getData = function () { return (.self$data) },
    
    getDimensionality = function () { return (length(.self$dims)) },
    
    getDimensions = function () { return (.self$dims) },
    
    setCoordinatesAndData = function (newCoords, newData)
    {
        .self$coords <- newCoords
        .self$data <- newData
    },
    
    setDimensions = function (newDims)
    {
        if (prod(.self$dims) != prod(newDims))
            report(OL$Error, "New dimensions are incompatible with this SparseArray object")
        .self$coords <- vectorToMatrixLocs(matrixToVectorLocs(.self$coords,.self$dims), newDims)
        .self$dims <- as.integer(newDims)
    }
))

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
    }
    else if (nArgs != (nDims + 1))
        report(OL$Error, "Number of dimensions given does not match image")
    else
    {
        # Find missing arguments and replace with NULL (code borrowed from the "slam" package)
        args <- substitute(list(i, j, ...))
        argsEmpty <- sapply(args, function(a) identical(as.character(a), ""))
        args[argsEmpty] <- list(NULL)
        args <- eval(args, envir=parent.frame())
        
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
            returnValue <- SparseArray$new(data=data[dataToKeep], coords=finalCoords[dataToKeep,dimsToKeep,drop=FALSE], dims=finalDims[dimsToKeep])
    }
    
    return (returnValue)
})

setReplaceMethod("[", "SparseArray", function (x, i, j, ..., value) {
    nArgs <- nargs() - 1
    
    nDims <- x$getDimensionality()
    dims <- x$getDimensions()
    data <- x$getData()
    coords <- x$getCoordinates()
    
    if (nArgs < 2)
        return (data)
    else if (nArgs == 2)
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
    else if (nArgs != (nDims + 1))
        report(OL$Error, "Number of dimensions given does not match image")
    else
    {
        args <- substitute(list(i, j, ...))
        argsEmpty <- sapply(args, function(a) identical(as.character(a), ""))
        args[argsEmpty] <- list(NULL)
        args <- eval(args, envir=parent.frame())
        
        args <- lapply(seq_len(nDims), function(i) {
            if (is.null(args[i]))
                1:dims[i]
            else
                args[i]
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
    zero <- (value == 0)
    
    data[present & !zero] <- value[present & !zero]
    if (any(present & zero))
    {
        coords <- coords[-which(present & zero),]
        data <- data[-which(present & zero)]
    }
    if (any(!present & !zero))
    {
        coords <- rbind(coords, arrayInd(index[!present & !zero],dims))
        data <- c(data, value[!present & !zero])
    }
    
    x$setCoordinatesAndData(coords, data)
    return (x)
})

setMethod("Arith", signature(e1="SparseArray",e2="numeric"), function (e1, e2) {
    if (callGeneric(0, e2) != 0)
        report(OL$Error, "Attempting to perform arithmetic on a sparse array which would not leave it sparse")
    newData <- callGeneric(e1$getData(), e2)
    zero <- (newData == 0)
    return (newSparseArrayWithData(newData[!zero], e1$getCoordinates()[!zero,], e1$getDimensions()))
})

setAs("array", "SparseArray", function (from) {
    coordinates <- which(!is.na(from) & from != 0, arr.ind=TRUE)
    object <- SparseArray$new(data=from[coordinates], coords=coordinates, dims=dim(from))
    return (object)
})

setAs("SparseArray", "array", function (from) {
    data <- array(vector(mode=storage.mode(from$getData()),length=1), dim=from$getDimensions())
    data[from$getCoordinates()] <- from$getData()
    return (data)
})

as.array.SparseArray <- function (x, ...)
{
    as(x, "array")
}

dim.SparseArray <- function (x)
{
    x$getDimensions()
}

"dim<-.SparseArray" <- function (x, value)
{
    x$setDimensions(value)
    return (x)
}

aperm.SparseArray <- function (a, perm, ...)
{
    newObject <- a$copy()$aperm(perm)
    return (newObject)
}

newSparseArrayWithData <- function (data, coordinates, dims)
{
    invisible (SparseArray$new(data=data, coords=coordinates, dims=dims))
}
