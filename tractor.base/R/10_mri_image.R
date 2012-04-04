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
        
        spatialUnit <- voxunit[voxunit %~% "m$"]
        temporalUnit <- voxunit[voxunit %~% "s$"]
        voxelDimString <- paste(implode(round(abs(voxdims[1:min(3,length(voxdims))]),5), sep=" x "), ifelse(length(spatialUnit)==1,paste(" ",spatialUnit,sep=""),""), sep="")
        if (length(voxdims) > 3)
            voxelDimString <- paste(voxelDimString, " x ", round(abs(voxdims[4]),5), ifelse(length(spatialUnit)==1 && length(temporalUnit)==1,paste(" ", temporalUnit,sep=""),""), sep="")
        if (length(voxdims) > 4)
            voxelDimString <- paste(voxelDimString, " x ", implode(round(abs(voxdims[5:length(voxdims)]),5), sep=" x "), sep="")
        if (identical(voxunit,"unknown"))
            voxelDimString <- paste(voxelDimString, "(units unknown)", sep=" ")
        
        labels <- c("Image source", "Image dimensions", "Coordinate origin", "Voxel dimensions", "Data type", "Additional tags")
        values <- c(source, paste(implode(imagedims, sep=" x "),"voxels",sep=" "), paste("(",implode(round(origin,2), sep=","),")",sep=""), voxelDimString, datatypeString, length(tags$keys))
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
    unitName <- from$getVoxelUnit()
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
    
    metadata <- MriImageMetadata$new(imagedims=from@dim_[seq_len(nDims)+1], voxdims=voxelDims, voxunit=voxelUnit, datatype=getDataTypeByNiftiCode(from@datatype), origin=c(1-origin/voxelDims3D,rep(0,max(0,3-nDims))))
    image <- newMriImageWithData(from@.Data, metadata)
    
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
        return (x$getData()[i,...,drop=drop])
    else
        return (x$getData()[i,j,...,drop=drop])
})

setMethod("[<-", "MriImage", function (x, i, j, ..., value) {
    if (missing(j))
        x$data[i,...] <- value
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
    if (is.null(dim(newData)))
        dim(newData) <- image$getDimensions()
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
