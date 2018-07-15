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
    datatype <- list(code=typeCode, type=.Nifti$datatypes$rTypes[typeIndex], size=.Nifti$datatypes$sizes[typeIndex], isSigned=.Nifti$datatypes$isSigned[typeIndex])
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

writeNifti <- function (image, fileNames, gzipped = FALSE, datatype = NULL, maxSize = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    description <- "TractoR NIfTI writer v3.0.0"
    fileFun <- (if (gzipped) gzfile else file)
    
    slope <- 1
    intercept <- 0
    dataRange <- range(image, na.rm=TRUE)
    dataImage <- image
    datatype <- chooseDataTypeForImage(image, "Nifti")
    if (!is.null(maxSize) && maxSize < datatype$size)
    {
        if (maxSize >= 4)
            datatype <- niftiDatatype(16)
        else
        {
            originalData <- as.array(image)
            if (any(is.na(originalData)))
                dataRange <- range(0, dataRange)
            
            datatype <- niftiDatatype(ifelse(maxSize >= 2, 4, 2))
            if (datatype$isSigned)
            {
                typeRange <- 2^(datatype$size*8-1) * c(-1,1) - c(0,1)
                slope <- diff(dataRange) / diff(typeRange)
                intercept <- -typeRange[1] * slope
            }
            else
            {
                typeRange <- c(0, 2^(datatype$size*8) - 1)
                slope <- diff(dataRange) / typeRange[2]
                intercept <- dataRange[1]
            }
            
            # NAs are typically not preserved when the data type is changed, so we replace them with zeros
            # The original image is replaced by its approximation; its source will be (re)set below
            dataImage <- image$copy()$map(function(x) as.integer(round((ifelse(is.na(x),0,x)-intercept)/slope)))
            image$map(function(x,y) y * slope + intercept, dataImage, sparse=image$isSparse())
            newData <- as.array(image)
            meanRelativeDifference <- mean(abs((newData-originalData) / originalData), na.rm=TRUE)
            if (meanRelativeDifference > 1e-4)
                report(OL$Warning, "Mean relative error in compressed image is #{meanRelativeDifference*100}%", round=2)
        }
    }
    
    ndims <- image$getDimensionality()
    fullDims <- c(ndims, image$getDimensions(), rep(1,7-ndims))
    fullVoxelDims <- c(-1, abs(image$getVoxelDimensions()), rep(0,7-ndims))
    
    # We default to 10 (mm and s)
    unitName <- image$getVoxelUnits()
    unitCode <- as.numeric(.Nifti$units[names(.Nifti$units) %in% unitName])
    if (length(unitCode) == 0)
        unitCode <- 10
    else
        unitCode <- sum(unitCode)
    
    xform <- image$getXform()
    sformRows <- c(xform[1,], xform[2,], xform[3,])
    quaternion <- xformToQuaternion(xform)
    fullVoxelDims[1] <- quaternion$handedness
    
    connection <- fileFun(fileNames$headerFile, "w+b")
    
    writeBin(as.integer(348), connection, size=4)
    writeBin(raw(36), connection)
    writeBin(as.integer(fullDims), connection, size=2)
    writeBin(raw(14), connection)
    writeBin(as.integer(datatype$code), connection, size=2)
    writeBin(as.integer(8*datatype$size), connection, size=2)
    writeBin(raw(2), connection)
    writeBin(fullVoxelDims, connection, size=4)
    
    # Voxel offset, data scaling slope and intercept
    writeBin(as.double(c(352,slope,intercept)), connection, size=4)
    
    writeBin(raw(3), connection)
    writeBin(as.integer(unitCode), connection, size=1)
    writeBin(as.double(rev(dataRange)), connection, size=4)
    writeBin(raw(16), connection)
    writeBin(charToRaw(description), connection, size=1)
    writeBin(raw(24+80-nchar(description)), connection)
    
    # NIfTI xform block: sform and qform codes are hardcoded to 2 here unless the image is 2D
    if (ndims == 2)
        writeBin(as.integer(c(0,0)), connection, size=2)
    else
        writeBin(as.integer(c(2,2)), connection, size=2)
    writeBin(quaternion$q[2:4], connection, size=4)
    writeBin(quaternion$offset, connection, size=4)
    writeBin(sformRows, connection, size=4)
    
    writeBin(raw(16), connection)
    if (fileNames$imageFile == fileNames$headerFile)
    {
        writeBin(c(charToRaw("n+1"),as.raw(0)), connection, size=1)
        writeBin(raw(4), connection)
    }
    else
    {
        writeBin(c(charToRaw("ni1"),as.raw(0)), connection, size=1)
        close(connection)
        connection <- fileFun(fileNames$imageFile, "w+b")
    }
    
    writeImageData(dataImage, connection, datatype$type, datatype$size)
    close(connection)
    
    if (image$isInternal())
        image$setSource(expandFileName(fileNames$fileStem))
}
