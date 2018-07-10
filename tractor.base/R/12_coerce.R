setOldClass(c("niftiImage", "internalImage"))

.convertNiftiImage <- function (from)
{
    # Pick up divest attributes and convert to tags (excl. patient info)
    attribs <- attributes(from)
    attribs <- attribs[!(names(attribs) %~% "^\\.|^(dim|imagedim|pixdim|pixunits|class|bValues|bVectors|reordered)$|^patient")]
    if (length(attribs) > 0)
        tags <- attribs
    else
        tags <- list()
    
    if (RNifti:::hasData(from))
    {
        data <- as.array(from)
        window <- range(data, na.rm=TRUE)
    }
    else
    {
        data <- NULL
        window <- c(0, 0)
    }
    
    metadata <- RNifti::dumpNifti(from)
    defaults <- list(dim_info=0, intent_p1=0, intent_p2=0, intent_p3=0, intent_code=0, intent_name="", slice_start=0, slice_end=0, slice_code=0, cal_min=window[1], cal_max=window[2], slice_duration=0, toffset=0, aux_file="")
    
    # Add NIfTI attributes that are set to something other than the defaults
    for (key in names(defaults))
    {
        if (metadata[[key]] != defaults[[key]])
            tags[[key]] <- metadata[[key]]
    }
    
    reordered <- attr(from, "reordered")
    if (is.null(reordered))
        reordered <- FALSE
    
    return (MriImage$new(imageDims=dim(from), RNifti::pixdim(from), voxelDimUnits=RNifti::pixunits(from), origin=RNifti::origin(from), storedXform=RNifti::xform(from), reordered=reordered, tags=tags, data=unclass(data)))
}

setAs("niftiImage", "MriImage", .convertNiftiImage)
setAs("internalImage", "MriImage", .convertNiftiImage)

setAs("MriImage", "nifti", function(from) {
    if (is.null(getOption("niftiAuditTrail")))
        options(niftiAuditTrail=FALSE)
    loadNamespace("oro.nifti")
    
    if (from$isEmpty())
    {
        datatype <- list(code=2, type="integer", size=1, isSigned=FALSE)
        data <- array(0L, dim=from$getDimensions())
    }
    else
    {
        datatype <- chooseDataTypeForImage(from, "Nifti")
        data <- as(from$getData(), "array")
    }
    
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
    
    xform <- from$getXform()
    sformRows <- c(xform[1,], xform[2,], xform[3,])
    quaternion <- xformToQuaternion(xform)
    fullVoxelDims[1] <- quaternion$handedness
    xformCode <- ifelse(from$getDimensionality() == 2, 0, 2)
    
    return (new(structure("nifti",package="oro.nifti"), .Data=data, dim_=fullDims, datatype=datatype$code, bitpix=8*datatype$size, pixdim=fullVoxelDims, xyzt_units=unitCode, qform_code=xformCode, sform_code=xformCode, quatern_b=quaternion$q[2], quatern_c=quaternion$q[3], quatern_d=quaternion$q[4], qoffset_x=quaternion$offset[1], qoffset_y=quaternion$offset[2], qoffset_z=quaternion$offset[3], srow_x=sformRows[1:4], srow_y=sformRows[5:8], srow_z=sformRows[9:12], cal_min=min(data), cal_max=max(data)))
})

setAs("nifti", "MriImage", function(from) as(RNifti::retrieveNifti(from), "MriImage"))

# MriImage methods for RNifti generics
#' @export
pixdim.MriImage <- function (object)
{
    object$getVoxelDimensions()
}

#' @export
"pixdim<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel dimensions cannot be changed at present")
}

#' @export
pixunits.MriImage <- function (object)
{
    object$getVoxelUnits()
}

#' @export
"pixunits<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel units cannot be changed at present")
}
