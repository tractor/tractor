setClassUnion("MriImageData", c("SparseArray","array","NULL"))

MriImage <- setRefClass("MriImage", contains="SerialisableObject", fields=list(imageDims="integer",voxelDims="numeric",voxelDimUnits="character",source="character",origin="numeric",storedXform="matrix",tags="list",data="MriImageData"), methods=list(
    initialize = function (imageDims = NULL, voxelDims = NULL, voxelDimUnits = NULL, source = "", origin = NULL, storedXform = matrix(NA,0,0), tags = list(), data = NULL, ...)
    {
        if (length(tags) != 0 && !all(c("keys","values") %in% names(tags)))
            report(OL$Error, "Tag list must be empty, or else contain \"keys\" and \"values\" components")
        if (is.null(voxelDimUnits))
            voxelDimUnits <- "unknown"
        
        # For backwards compatibility
        if (source == "internal")
            source <- ""
        
        if (!is.null(imageDims) && !is.null(data) && !equivalent(imageDims,dim(data)))
            dim(data) <- imageDims
        else if (is.null(imageDims))
            imageDims <- dim(data)
        
        # For backwards compatibility
        if (is.null(voxelDims) && "voxdims" %in% names(list(...)))
        {
            oldFields <- list(...)
            if (is.null(oldFields$voxunit))
                oldFields$voxunit <- "unknown"
            object <- initFields(imageDims=as.integer(oldFields$imagedims), voxelDims=as.numeric(oldFields$voxdims), voxelDimUnits=oldFields$voxunit, source=source, origin=as.numeric(origin), storedXform=storedXform, tags=tags, data=data)
        }
        else
            object <- initFields(imageDims=as.integer(imageDims), voxelDims=as.numeric(voxelDims), voxelDimUnits=voxelDimUnits, source=source, origin=as.numeric(origin), storedXform=as.matrix(storedXform), tags=tags, data=data)
        
        names(object$voxelDimUnits)[object$voxelDimUnits %~% "m$"] <- "spatial"
        names(object$voxelDimUnits)[object$voxelDimUnits %~% "s$"] <- "temporal"
        
        return (object)
    },
    
    apply = function (...)
    {
        if (.self$isEmpty())
            report(OL$Error, "The image contains no data")
        else if (.self$isSparse())
            return (data$apply(...))
        else
            return (base::apply(data, ...))
    },
    
    getData = function () { return (data) },
    
    getDataAtPoint = function (...)
    {
        if (is.null(data))
            return (NA)
        
        dim <- getDimensionality()
        loc <- resolveVector(len=dim, ...)
        if (is.null(loc) || (length(...) != dim))
            report(OL$Error, "Point must be specified as a ", dim, "-vector")
            
        if (all(loc >= 1) && all(loc <= getDimensions()))
            return (data[matrix(loc,nrow=1)])
        else
            return (NA)
    },
    
    getDimensionality = function () { return (length(voxelDims)) },
    
    getDimensions = function () { return (imageDims) },
    
    getFieldOfView = function () { return (abs(voxelDims) * imageDims) },
    
    getNonzeroIndices = function (array = TRUE, positiveOnly = FALSE)
    {
        if (.self$isEmpty())
            report(OL$Error, "The image contains no data")
        else if (.self$isSparse())
        {
            locs <- data$getCoordinates()
            if (positiveOnly)
                locs <- locs[data$getData() > 0,]
            if (array)
                return (locs)
            else
                return (matrixToVectorLocs(locs, data$getDimensions()))
        }
        else
        {
            if (positiveOnly)
                return (which(data > 0, arr.ind=array))
            else
                return (which(data != 0, arr.ind=array))
        }
    },
    
    getOrigin = function () { return (origin) },
    
    getSource = function () { return (source) },
    
    getSparseness = function ()
    {
        if (.self$isEmpty())
            return (NA)
        else if (.self$isSparse())
            return (1 - (nrow(data$getCoordinates()) / prod(.self$getDimensions())))
        else
            return (sum(data==0 | is.na(data)) / prod(.self$getDimensions()))
    },
    
    getStoredXformMatrix = function () { return (storedXform) },
    
    getTag = function (key)
    {
        if (!is.null(tags$keys) && !is.null(tags$values) && any(key == tags$keys))
        {
            rawValues <- tags$values[which(key == tags$keys)]
            return (strsplit(implode(rawValues,sep=""), "\\s*\\\\\\s*", perl=TRUE))
        }
        else
            return (NA_character_)
    },
    
    getTags = function () { return (tags) },
    
    getVoxelDimensions = function () { return (voxelDims) },
    
    getVoxelUnits = function () { return (voxelDimUnits) },
    
    isEmpty = function () { return (is.null(data)) },
    
    isInternal = function () { return (source == "") },
    
    isSparse = function () { return (is(data,"SparseArray")) },
    
    setOrigin = function (newOrigin)
    {
        if (is.numeric(newOrigin) && length(newOrigin) == .self$getDimensionality())
        {
            .self$origin <- newOrigin
            .self$setSource(NULL)
        }
    },
    
    setSource = function (newSource)
    {
        if (is.null(newSource))
            .self$source <- ""
        else if (is.character(newSource) && (length(newSource) == 1))
            .self$source <- newSource
    },
    
    stripData = function () { .self$data <- NULL },
    
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
        
        labels <- c("Image source", "Image dimensions", "Voxel dimensions", "Coordinate origin", "Additional tags")
        values <- c(ifelse(source=="","internal",source), paste(implode(imageDims, sep=" x "),"voxels",sep=" "), voxelDimString, paste("(",implode(round(origin,2), sep=","),")",sep=""), length(tags$keys))
        
        if (!.self$isEmpty())
        {
            sparseness <- paste(round(.self$getSparseness()*100,2), "% (", ifelse(.self$isSparse(),"sparse","dense"), " storage)", sep="")
            labels <- c(labels, "Sparseness")
            values <- c(values, sparseness)
        }
        
        return (list(labels=labels, values=values))
    }
))

# Register deserialiser for MriImageMetadata legacy class
registerDeserialiser("MriImageMetadata", function (fields) {
    object <- MriImage$new(imageDims=fields$imagedims, voxelDims=fields$voxdims, voxelDimUnits=fields$voxunit, source=fields$source, origin=fields$origin, storedXform=fields$storedXform, tags=fields$tags, data=NULL)
    return (object)
})

setAs("MriImage", "array", function (from) as(from$getData(),"array"))

setAs("array", "MriImage", function (from) newMriImageWithData(from))

setAs("MriImage", "nifti", function (from) {
    if (is.null(getOption("niftiAuditTrail")))
        options(niftiAuditTrail=FALSE)
    suppressPackageStartupMessages(require(oro.nifti))
    
    datatype <- from$getDataType()
    datatypeMatches <- (.Nifti$datatypes$rTypes == datatype$type) & (.Nifti$datatypes$sizes == datatype$size) & (.Nifti$datatypes$isSigned == datatype$isSigned)
    if (length(which(datatypeMatches)) != 1)
        report(OL$Error, "No supported NIfTI datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- as(from$getData(), "array")
    storage.mode(data) <- .Nifti$datatypes$rTypes[typeIndex]
    
    # We default to 10 (mm and s)
    unitName <- from$getVoxelUnits()
    unitCode <- as.numeric(.Nifti$units[names(.Nifti$units) %in% unitName])
    if (length(unitCode) == 0)
        unitCode <- 10
    else
        unitCode <- sum(unitCode)
    
    nDims <- from$getDimensionality()
    fullDims <- c(nDims, abs(from$getDimensions()), rep(1,7-nDims))
    fullVoxelDims <- c(-1, abs(from$getVoxelDimensions()), rep(0,7-nDims))
    
    origin <- (from$getOrigin() - 1) * abs(from$getVoxelDimensions())
    if (length(origin) > 3)
        origin <- origin[1:3]
    else if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    origin <- ifelse(origin < 0, rep(0,3), origin)
    origin[2:3] <- -origin[2:3]
    sformRows <- c(-fullVoxelDims[2], 0, 0, origin[1],
                    0, fullVoxelDims[3], 0, origin[2],
                    0, 0, fullVoxelDims[4], origin[3])
    
    xformCode <- ifelse(from$getDimensionality() == 2, 0, 2)
    
    return (new("nifti", .Data=data, dim_=fullDims, datatype=.Nifti$datatypes$codes[typeIndex], bitpix=8*.Nifti$datatypes$sizes[typeIndex], pixdim=fullVoxelDims, xyzt_units=unitCode, qform_code=xformCode, sform_code=xformCode, quatern_b=0, quatern_c=1, quatern_d=0, qoffset_x=origin[1], qoffset_y=origin[2], qoffset_z=origin[3], srow_x=sformRows[1:4], srow_y=sformRows[5:8], srow_z=sformRows[9:12], cal_min=min(data), cal_max=max(data)))
})

setAs("nifti", "MriImage", function (from) {
    if (is.null(getOption("niftiAuditTrail")))
        options(niftiAuditTrail=FALSE)
    suppressPackageStartupMessages(require(oro.nifti))
    
    nDims <- from@dim_[1]
    voxelDims <- from@pixdim[seq_len(nDims)+1]
    voxelDims3D <- c(voxelDims, rep(0,max(0,3-nDims))) * c(-1,1,1)
    
    spatialUnitCode <- packBits(intToBits(from@xyzt_units) & intToBits(7), "integer")
    temporalUnitCode <- packBits(intToBits(from@xyzt_units) & intToBits(24), "integer")
    voxelUnit <- names(.Nifti$units)[.Nifti$units %in% c(spatialUnitCode,temporalUnitCode)]
    if (length(voxelUnit) == 0)
        voxelUnit <- NULL
    
    if (from@qform_code > 0)
    {
        if (!equivalent(c(from@quatern_b,from@quatern_c,from@quatern_d), c(0,1,0)))
            report(OL$Error, "Only images using the LAS orientation convention can be converted at present")
        origin <- c(from@qoffset_x, from@qoffset_y, from@qoffset_z)
    }
    else if (from@sform_code > 0)
    {
        if (!equivalent(c(from@srow_x[1:3],from@srow_y[1:3],from@srow_z[1:3]), c(-voxelDims3D[1],0,0,0,voxelDims3D[2],0,0,0,voxelDims3D[3])))
            report(OL$Error, "Only images using the LAS orientation convention can be converted at present")
        origin <- c(from@srow_x[4], from@srow_y[4], from@srow_z[4])
    }
    else
        origin <- rep(0, 3)
    
    image <- newMriImageWithData(from@.Data, imageDims=from@dim_[seq_len(nDims)+1], voxelDims=voxelDims, voxelDimUnits=voxelUnit, origin=c(1-origin/voxelDims3D,rep(0,max(0,3-nDims))))
    return (image)
})

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

setMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., drop = TRUE) {
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 2)
        return (x$getData())
    else
        return (x$getData()[,,...,drop=drop])
})

setMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., drop = TRUE) {
    nArgs <- nargs() - as.integer(!missing(drop))
    if (nArgs < 3)
        return (x$getData()[i,drop=drop])
    else
        return (x$getData()[i,,...,drop=drop])
})

setMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    return (x$getData()[,j,...,drop=drop])
})

setMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., drop = TRUE) {
    return (x$getData()[i,j,...,drop=drop])
})

setReplaceMethod("[", signature(x="MriImage",i="missing",j="missing"), function (x, i, j, ..., value) {
    nArgs <- nargs() - 1
    if (nArgs < 2)
        x$data[] <- value
    else
        x$data[,,...] <- value
    x$setSource("internal")
    return (x)
})

setReplaceMethod("[", signature(x="MriImage",i="ANY",j="missing"), function (x, i, j, ..., value) {
    nArgs <- nargs() - 1
    if (nArgs < 3)
        x$data[i] <- value
    else
        x$data[i,,...] <- value
    x$setSource("internal")
    return (x)
})

setReplaceMethod("[", signature(x="MriImage",i="missing",j="ANY"), function (x, i, j, ..., value) {
    x$data[,j,...] <- value
    x$setSource("internal")
    return (x)
})

setReplaceMethod("[", signature(x="MriImage",i="ANY",j="ANY"), function (x, i, j, ..., value) {
    x$data[i,j,...] <- value
    x$setSource("internal")
    return (x)
})

# setMethod("Math", "MriImage", Math.MriImage)

setMethod("Ops", "MriImage", Ops.MriImage)

setMethod("Summary", "MriImage", Summary.MriImage)

newMriImageWithData <- function (data, templateImage = nilObject(), imageDims = NA, voxelDims = NA, voxelDimUnits = NA, origin = NA, tags = NA)
{
    if (is.null(data))
        report(OL$Error, "Data may not be NULL")
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
    
    defaults <- list(voxelDims=rep(1,nDims), voxelDimUnits="unknown", origin=c(1,1,1,0,0,0,0), tags=list())
    template <- templateImage$serialise()
    params <- list(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=voxelDimUnits, origin=origin, tags=tags)
    params <- params[!is.na(params)]
    
    composite <- c(params, template, defaults)
    composite <- composite[!duplicated(names(composite))]
    
    image <- MriImage$new(imageDims=composite$imageDims[1:nDims], voxelDims=composite$voxelDims[1:nDims], voxelDimUnits=composite$voxelDimUnits, origin=composite$origin[1:nDims], tags=composite$tags, data=data)
    invisible (image)
}

newMriImageWithDataRepresentation <- function (image, representation = c("dense","coordlist"))
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not an MriImage object")
    if (image$isEmpty())
        return (image)
    
    representation <- match.arg(representation)
    
    if (image$isSparse() && representation == "dense")
        newImage <- newMriImageWithData(as(image$getData(), "array"), image$getMetadata())
    else if (!image$isSparse() && representation == "coordlist")
        newImage <- newMriImageWithData(as(image$getData(), "SparseArray"), image$getMetadata())
    else
        newImage <- image
    
    return (newImage)
}

newMriImageWithSimpleFunction <- function (image, fun, ...)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    fun <- match.fun(fun)
    newData <- fun(image$getData(), ...)
    return (newMriImageWithData(newData, image))
}

newMriImageWithBinaryFunction <- function (image1, image2, fun, ...)
{
    if (!is(image1,"MriImage") || !is(image2,"MriImage"))
        report(OL$Error, "The specified images are not MriImage objects")
    
    fun <- match.fun(fun)
    newData <- fun(image1$getData(), image2$getData(), ...)
    return (newMriImageWithData(newData, image1))
}

extractDataFromMriImage <- function (image, dim, loc)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() < max(2,dim))
        report(OL$Error, "The dimensionality of the specified image is too low")
    if (!(loc %in% 1:(image$getDimensions()[dim])))
        report(OL$Error, "The specified location is out of bounds")
    
    # This "apply" call is a cheeky bit of R wizardry (credit: Peter Dalgaard)
    dimsToKeep <- setdiff(1:image$getDimensionality(), dim)
    newData <- image$apply(dimsToKeep, "[", loc)
    if (is.vector(newData))
        newData <- promote(newData)
    
    invisible (newData)
}

newMriImageByExtraction <- function (image, dim, loc)
{
    newData <- extractDataFromMriImage(image, dim, loc)
    dimsToKeep <- setdiff(1:image$getDimensionality(), dim)
    
    image <- newMriImageWithData(newData, image, imageDims=image$getDimensions()[dimsToKeep], voxelDims=image$getVoxelDimensions()[dimsToKeep], origin=image$getOrigin()[dimsToKeep])
    return (image)
}

newMriImageByMasking <- function (image, mask)
{
    if (!identical(image$getDimensions(), dim(mask)))
        report(OL$Error, "The specified image and mask do not have the same dimensions")
    if (!is.logical(mask))
        mask <- mask != 0
    
    newData <- image$getData() * mask
    newImage <- newMriImageWithData(newData, image)
    invisible (newImage)
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
    newImage <- newMriImageWithData(data, image, imageDims=newDims)
    invisible (newImage)
}
