setOldClass(c("niftiImage", "internalImage"))

.convertNiftiImage <- function (from)
{
    metadata <- dumpNifti(from)
    defaults <- list(dim_info=0, intent_p1=0, intent_p2=0, intent_p3=0, intent_code=0, intent_name="", slice_start=0, slice_end=0, slice_code=0, cal_min=0, cal_max=0, slice_duration=0, toffset=0, aux_file="")
    
    tags <- list()
    for (key in names(defaults))
    {
        if (metadata[[key]] != defaults[[key]])
            tags[[key]] <- metadata[[key]]
    }
    
    data <- as.array(from)
    if (metadata$scl_slope != 1 || metadata$scl_inter != 0)
        data <- data * metadata$scl_slope + metadata$scl_inter
    
    image <- asMriImage(unclass(data), imageDims=dim(from), voxelDims=pixdim(from), voxelDimUnits=pixunits(from), origin=worldToVoxel(c(0,0,0),from), tags=tags, reordered=FALSE)
    image$setXform(xform(from))
    
    return (image)
}

setAs("niftiImage", "MriImage", .convertNiftiImage)
setAs("internalImage", "MriImage", .convertNiftiImage)

# MriImage methods for RNiftyReg generics
pixdim.MriImage <- function (object)
{
    object$getVoxelDimensions()
}

"pixdim<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel dimensions cannot be changed at present")
}

pixunits.MriImage <- function (object)
{
    object$getVoxelUnits()
}

"pixunits<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel units cannot be changed at present")
}
