setOldClass(c("niftiImage", "internalImage"))

.convertNiftiImage <- function (from)
{
    # Pick up divest attributes and convert to tags (excl. patient info)
    attribs <- attributes(from)
    attribs <- attribs[!(names(attribs) %~% "^\\.|^(dim|imagedim|pixdim|pixunits|class|reordered)$|^patient")]
    if (length(attribs) > 0)
        tags <- attribs
    else
        tags <- list()
    
    if (RNifti:::hasData(from))
        data <- as.array(from)
    else
        data <- NULL
    
    metadata <- niftiHeader(from)
    defaults <- list(dim_info=0, intent_p1=0, intent_p2=0, intent_p3=0, intent_code=0, intent_name="", slice_start=0, slice_end=0, slice_code=0, cal_min=0, cal_max=0, slice_duration=0, toffset=0, aux_file="")
    
    # Add NIfTI attributes that are set to something other than the defaults
    for (key in names(defaults))
    {
        if (metadata[[key]] != defaults[[key]])
            tags[[key]] <- metadata[[key]]
    }
    
    reordered <- attr(from, "reordered")
    if (is.null(reordered))
        reordered <- FALSE
    
    return (MriImage$new(imageDims=dim(from), pixdim(from), voxelDimUnits=pixunits(from), origin=origin(from), xform=xform(from), reordered=reordered, tags=tags, data=unclass(data)))
}

setAs("niftiImage", "MriImage", .convertNiftiImage)
setAs("internalImage", "MriImage", .convertNiftiImage)

setAs("MriImage", "nifti", function(from) {
    if (is.null(getOption("niftiAuditTrail")))
        options(niftiAuditTrail=FALSE)
    
    nifti <- as.array(retrieveNifti(from))
    if (RNifti:::hasData(nifti))
        nifti <- updateNifti(nifti, list(cal_min=min(nifti,na.rm=TRUE), cal_max=max(nifti,na.rm=TRUE)))
    
    return (oro.nifti::nii2oro(nifti))
})

setAs("nifti", "MriImage", function(from) as(retrieveNifti(from), "MriImage"))

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
