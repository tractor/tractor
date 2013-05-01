# The standard "nifti" validity method is too restrictive for our purposes, so we redefine it
setValidity("nifti", function (object) {
    errors <- character(0)
    dims <- object@dim_[1 + 1:object@dim_[1]]
    
    if (object@sizeof_hdr != 348)
        errors <- c(errors, "Header size must be 348")
    if (!(object@datatype %in% convert.datatype()))
        errors <- c(errors, "Data type is not valid")
    if (object@bitpix != convert.bitpix()[[convert.datatype(object@datatype)]])
        errors <- c(errors, "Bits per pixel does not match the data type")
    
    # This is the key change: allow for empty data
    if (!identical(drop(object@.Data),NA) && !isTRUE(all.equal(dims,dim(object@.Data))))
        errors <- c(errors, "Data is nonempty and dimensions don't match metadata")
    
    if (length(errors) == 0)
        return (TRUE)
    else
        return (errors)
})

# We also need a custom "dim" method, which uses the metadata rather than the data
dim.nifti <- function (x)
{
    nDims <- x@dim_[1]
    return (x@dim_[1 + 1:nDims])
}

# Redefine coercion methods from tractor.base to convert empty images efficiently
setAs("MriImage", "nifti", function (from) {
    if (from$isEmpty())
    {
        datatype <- list(code=2, type="integer", size=1, isSigned=FALSE)
        data <- array(NA, dim=rep(1,from$getDimensionality()))
    }
    else
    {
        datatype <- chooseDataTypeForImage(from, "Nifti")
        data <- as(from$getData(), "array")
    }
    
    # We default to 10 (mm and s)
    unitName <- from$getVoxelUnits()
    unitCode <- as.numeric(tractor.base:::.Nifti$units[names(tractor.base:::.Nifti$units) %in% unitName])
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
    
    return (new(structure("nifti",package="oro.nifti"), .Data=data, dim_=fullDims, datatype=datatype$code, bitpix=8*datatype$size, pixdim=fullVoxelDims, xyzt_units=unitCode, qform_code=xformCode, sform_code=xformCode, quatern_b=0, quatern_c=1, quatern_d=0, qoffset_x=origin[1], qoffset_y=origin[2], qoffset_z=origin[3], srow_x=sformRows[1:4], srow_y=sformRows[5:8], srow_z=sformRows[9:12], cal_min=min(data), cal_max=max(data)))
})

setAs("nifti", "MriImage", function (from) {
    nDims <- from@dim_[1]
    voxelDims <- from@pixdim[seq_len(nDims)+1]
    voxelDims3D <- c(voxelDims, rep(0,max(0,3-nDims)))[1:3] * c(-1,1,1)
    
    spatialUnitCode <- packBits(intToBits(from@xyzt_units) & intToBits(7), "integer")
    temporalUnitCode <- packBits(intToBits(from@xyzt_units) & intToBits(24), "integer")
    voxelUnit <- names(tractor.base:::.Nifti$units)[tractor.base:::.Nifti$units %in% c(spatialUnitCode,temporalUnitCode)]
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
