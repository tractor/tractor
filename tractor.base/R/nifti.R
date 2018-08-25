hasNiftiMagicString <- function (fileName)
{
    connection <- gzfile(fileName, "rb")
    readBin(connection, "raw", n=344)
    magicString <- readBin(connection, "raw", n=4)
    close(connection)
    
    return (any(sapply(unlist(.Nifti$magicStrings,recursive=FALSE), identical, magicString)))
}

niftiDatatype <- function (typeCode)
{
    typeIndex <- which(.Nifti$datatypes$codes == typeCode)
    if (length(typeIndex) != 1)
        report(OL$Error, "NIfTI data type code #{typeCode} is not supported")
    datatype <- list(code=typeCode, type=.Nifti$datatypes$rTypes[typeIndex], size=.Nifti$datatypes$sizes[typeIndex], isSigned=.Nifti$datatypes$isSigned[typeIndex], name=.Nifti$datatype$names[typeIndex])
    return (datatype)
}

readNifti <- function (fileNames, volumes = NULL)
{
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "Header file #{fileNames$headerFile} not found")
    
    # Return value
    result <- list(image=NULL, header=NULL, storage=NULL)
    
    version <- niftiVersion(fileNames$headerFile)
    if (version < 0)
        report(OL$Error, "#{fileNames$headerFile} does not seem to be a valid NIfTI header file")
    else if (version == 0)
    {
        analyze <- analyzeHeader(fileNames$headerFile)
        
        flag(OL$Warning, "Image orientation for ANALYZE format is inconsistently interpreted")
        xform <- diag(c(-abs(analyze$pixdim[2]), analyze$pixdim[3:4], 1))
        if (analyze$orient > 0 && analyze$orient < 6)
            orientation(xform) <- switch(analyze$orient, "LSA", "ASL", "LPS", "LIA", "AIL")
        xform[1:3,4] <- -(xform[1:3,1:3] %*% (analyze$origin[1:3]-1))
        
        result$image <- RNifti::readNifti(fileNames$headerFile, volumes=volumes)
        qform(result$image) <- structure(xform, code=2L)
    }
    else if (version == 2)
    {
        # The gzfile function can handle uncompressed files too
        connection <- gzfile(fileNames$headerFile, "rb")
        
        # Read header size and check endianness
        size <- readBin(connection, "integer", n=1, size=4)
        nonNativeEndian <- setdiff(c("big","little"), .Platform$endian)
        endian <- switch(as.character(size), "540"=.Platform$endian, "469893120"=nonNativeEndian)
        if (is.null(endian))
            report(OL$Error, "#{fileNames$headerFile} does not seem to be a valid NIfTI header file")
        else
            report(OL$Debug, "NIfTI-2 file, #{endian}-endian")
        
        # Read and check magic number
        magicString <- readBin(connection, "raw", n=4)
        if (sum(sapply(.Nifti$magicStrings[[2]], identical, magicString)) != 1)
            report(OL$Error, "The file #{fileNames$headerFile} does not have a valid NIfTI-2 magic number")
        
        readBin(connection, "raw", n=4)     # last 4 bytes of the magic number
        typeCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
        readBin(connection, "raw", n=2)     # bits per pixel - we determine this from the datatype
        dims <- readBin(connection, "integer", n=8, size=8, endian=endian)
        intentParams <- readBin(connection, "double", n=3, size=8, endian=endian)
        voxelDims <- readBin(connection, "double", n=8, size=8, endian=endian)
        dataOffset <- readBin(connection, "integer", n=1, size=8, endian=endian)
        slopeAndIntercept <- readBin(connection, "double", n=2, size=8, endian=endian)
        windowLimits <- readBin(connection, "double", n=2, size=8, endian=endian)
        sliceDuration <- readBin(connection, "double", n=1, size=8, endian=endian)
        timeOffset <- readBin(connection, "double", n=1, size=8, endian=endian)
        firstAndLastSlice <- readBin(connection, "integer", n=2, size=8, endian=endian)
        description <- rawToChar(stripNul(readBin(connection, "raw", n=80)))
        auxFile <- rawToChar(stripNul(readBin(connection, "raw", n=24)))
        qformCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        sformCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        quatern <- readBin(connection, "double", n=6, size=8, endian=endian)
        sformRows <- readBin(connection, "double", n=12, size=8, endian=endian)
        sliceCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        unitCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        intentCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        intentName <- rawToChar(stripNul(readBin(connection, "raw", n=16)))
        dimInfo <- readBin(connection, "integer", n=1, size=1, endian=endian)
        
        close(connection)
        
        result$header <- list(dim=dims, intent_p1=intentParams[1], intent_p2=intentParams[2], intent_p3=intentParams[3], pixdim=voxelDims, cal_max=windowLimits[1], cal_min=windowLimits[2], slice_duration=sliceDuration, toffset=timeOffset, slice_start=firstAndLastSlice[1], slice_end=firstAndLastSlice[2], descrip=description, aux_file=auxFile, qform_code=qformCode, sform_code=sformCode, quatern_b=quatern[1], quatern_c=quatern[2], quatern_d=quatern[3], qoffset_x=quatern[4], qoffset_y=quatern[5], qoffset_z=quatern[6], srow_x=sformRows[1:4], srow_y=sformRows[5:8], srow_z=sformRows[9:12], slice_code=sliceCode, xyzt_units=unitCode, intent_code=intentCode, intent_name=intentName, dim_info=dimInfo)
        
        result$storage <- list(datatype=niftiDatatype(typeCode), endian=endian, offset=dataOffset, slope=slopeAndIntercept[1], intercept=slopeAndIntercept[2])
    }
    else
        result$image <- RNifti::readNifti(fileNames$headerFile, volumes=volumes)
    
    invisible (result)
}

writeNifti <- function (image, fileNames, maxSize = NULL)
{
    datatype <- chooseDataTypeForImage(image, "Nifti")
    if (!is.null(maxSize))
        datatype <- niftiDatatype(ifelse(maxSize >= 4, 16L, ifelse(maxSize >= 2, 4L, 2L)))
    
    image <- retrieveNifti(image)
    image <- updateNifti(image, list(descrip="TractoR NIfTI writer v3.3.0"))
    
    RNifti::writeNifti(image, fileNames$headerFile, datatype=datatype$name)
}
