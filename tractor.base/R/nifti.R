getDataTypeByNiftiCode <- function (code)
{
    typeIndex <- which(.Nifti$datatypes$codes == code)
    if (length(typeIndex) != 1)
        report(OL$Error, "Specified NIfTI data type code is not valid")
    datatype <- list(type=.Nifti$datatypes$rTypes[typeIndex], size=.Nifti$datatypes$sizes[typeIndex], isSigned=.Nifti$datatypes$isSigned[typeIndex])
    
    return (datatype)
}

hasNiftiMagicString <- function (fileName)
{
    connection <- gzfile(fileName, "rb")
    readBin(connection, "raw", n=344)
    magicString <- readBin(connection, "raw", n=4)
    close(connection)
    
    return (any(sapply(unlist(.Nifti$magicStrings,recursive=FALSE), identical, magicString)))
}

readNifti <- function (fileNames)
{
    getXformMatrix <- function ()
    {
        # With no information, assume Analyze orientation and zero origin
        if (qformCode <= 0 && sformCode <= 0)
        {
            report(OL$Warning, "Nifti qform and sform codes are both zero in file ", fileNames$headerFile, " - orientation can only be guessed")
            return (diag(c(-abs(voxelDims[2]), abs(voxelDims[3:4]), 1)))
        }
        else if (qformCode > 0)
        {
            report(OL$Debug, "Using qform (code ", qformCode, ") for origin")
            matrix <- diag(4)
            matrix[1:3,4] <- quaternionParams[4:6]
            q <- c(sqrt(1 - sum(quaternionParams[1:3]^2)), quaternionParams[1:3])

            matrix[1:3,1:3] <- c(  q[1]*q[1] +   q[2]*q[2] - q[3]*q[3] - q[4]*q[4],
                                 2*q[2]*q[3] + 2*q[1]*q[4],
                                 2*q[2]*q[4] - 2*q[1]*q[3],
                                 2*q[2]*q[3] - 2*q[1]*q[4],
                                   q[1]*q[1] +   q[3]*q[3] - q[2]*q[2] - q[4]*q[4],
                                 2*q[3]*q[4] + 2*q[1]*q[2],
                                 2*q[2]*q[4] + 2*q[1]*q[3],
                                 2*q[3]*q[4] - 2*q[1]*q[2],
                                   q[1]*q[1] +   q[4]*q[4] - q[3]*q[3] - q[2]*q[2])

            # The qfactor should be stored as 1 or -1, but the NIfTI standard says
            # 0 should be treated as 1; this does that (the 0.1 is arbitrary)
            qfactor <- sign(voxelDims[1] + 0.1)
            matrix[1:3,1:3] <- matrix[1:3,1:3] * c(abs(voxelDims[2:3]), qfactor*abs(voxelDims[4]))

            return (matrix)
        }
        else
        {
            report(OL$Debug, "Using sform (code ", sformCode, ") for origin")
            return (rbind(affine, c(0,0,0,1)))
        }
    }
    
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "Header file ", fileNames$headerFile, " not found")
        
    # The gzfile function can handle uncompressed files too
    connection <- gzfile(fileNames$headerFile, "rb")
    
    size <- readBin(connection, "integer", n=1, size=4)
    
    nonNativeEndian <- setdiff(c("big","little"), .Platform$endian)
    endian <- switch(as.character(size), "348"=.Platform$endian, "540"=.Platform$endian, "23553"=nonNativeEndian, "7170"=nonNativeEndian)
    niftiVersion <- switch(as.character(size), "348"=1, "540"=2, "23553"=1, "7170"=2)

    if (niftiVersion == 1)
    {
        readBin(connection, "raw", n=36)
        dims <- readBin(connection, "integer", n=8, size=2, endian=endian)
        readBin(connection, "raw", n=14)
        typeCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
        readBin(connection, "raw", n=4)
        voxelDims <- readBin(connection, "double", n=8, size=4, endian=endian)
        dataOffset <- readBin(connection, "double", n=1, size=4, endian=endian)
        slopeAndIntercept <- readBin(connection, "double", n=2, size=4, endian=endian)
        readBin(connection, "raw", n=3)
        unitCode <- readBin(connection, "integer", n=1, size=1, endian=endian)
        readBin(connection, "raw", n=128)
        qformCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
        sformCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
        quaternionParams <- readBin(connection, "double", n=6, size=4, endian=endian)
        affine <- matrix(readBin(connection, "double", n=12, size=4, endian=endian), nrow=3, byrow=TRUE)
        readBin(connection, "raw", n=16)
        magicString <- readBin(connection, "raw", n=4)
    }
    else
    {
        magicString <- readBin(connection, "raw", n=4)
        readBin(connection, "raw", n=4)
        typeCode <- readBin(connection, "integer", n=1, size=2, endian=endian)
        readBin(connection, "raw", n=2)
        dims <- readBin(connection, "integer", n=8, size=8, endian=endian)
        readBin(connection, "raw", n=24)
        voxelDims <- readBin(connection, "double", n=8, size=8, endian=endian)
        dataOffset <- readBin(connection, "integer", n=1, size=8, endian=endian)
        slopeAndIntercept <- readBin(connection, "double", n=2, size=8, endian=endian)
        readBin(connection, "raw", n=152)
        qformCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        sformCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
        quaternionParams <- readBin(connection, "double", n=6, size=8, endian=endian)
        affine <- matrix(readBin(connection, "double", n=12, size=8, endian=endian), nrow=3, byrow=TRUE)
        readBin(connection, "raw", n=4)
        unitCode <- readBin(connection, "integer", n=1, size=4, endian=endian)
    }
    
    close(connection)
    
    # Require exactly one match to a magic string suitable to the NIfTI version
    if (sum(sapply(.Nifti$magicStrings[[niftiVersion]], identical, magicString)) != 1)
        report(OL$Error, "The file ", fileNames$headerFile, " is not a valid NIfTI file")
    
    ndims <- dims[1]
    dims <- dims[1:ndims + 1]
    
    xformMatrix <- getXformMatrix()
    
    typeIndex <- which(.Nifti$datatypes$codes == typeCode)
    if (length(typeIndex) != 1)
        report(OL$Error, "Data type of file ", fileNames$imageFile, " (", typeCode, ") is not supported")
    datatype <- getDataTypeByNiftiCode(typeCode)
    
    # We're only interested in the bottom 3 bits (the spatial unit)
    unitCode <- packBits(c(intToBits(unitCode)[1:3], intToBits(0L)[4:32]), "integer")
    unit <- names(.Nifti$units)[which(.Nifti$units == unitCode)]
    if (length(unit) == 0)
        unit <- NULL
    
    dimsToKeep <- 1:max(which(dims > 1))
    voxelDims[2:4] <- voxelDims[2:4] * sign(diag(xformMatrix)[1:3])
    
    imageMetadata <- list(imageDims=dims[dimsToKeep], voxelDims=voxelDims[dimsToKeep+1], voxelUnit=unit, source=fileNames$fileStem, datatype=datatype, tags=list())
    
    storageMetadata <- list(dataOffset=dataOffset, dataScalingSlope=slopeAndIntercept[1], dataScalingIntercept=slopeAndIntercept[2], xformMatrix=xformMatrix, endian=endian)
    
    invisible (list(imageMetadata=imageMetadata, storageMetadata=storageMetadata))
}

writeMriImageToNifti <- function (image, fileNames, gzipped = FALSE, datatype = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    description <- "TractoR NIfTI writer v1.0.1"
    fileFun <- (if (gzipped) gzfile else file)
    
    if (is.null(datatype))
    {
        datatype <- image$getDataType()
        if (is.null(datatype))
            report(OL$Error, "The data type is not stored with the image; it must be specified")
    }
    
    datatypeMatches <- (.Nifti$datatypes$rTypes == datatype$type) & (.Nifti$datatypes$sizes == datatype$size) & (.Nifti$datatypes$isSigned == datatype$isSigned)
    if (length(which(datatypeMatches)) != 1)
        report(OL$Error, "No supported NIfTI datatype is appropriate for this file")
    typeIndex <- which(datatypeMatches)
    
    data <- image$getData()
    if (.Nifti$datatypes$rTypes[typeIndex] == "integer")
        data <- as.integer(data)
    else
        data <- as.double(data)
    
    ndims <- image$getDimensionality()
    fullDims <- c(ndims, image$getDimensions(), rep(1,7-ndims))
    fullVoxelDims <- c(-1, abs(image$getVoxelDimensions()), rep(0,7-ndims))
    
    # The 8 below is for seconds; we default to 10 (mm/sec)
    unitName <- image$getVoxelUnit()
    unitCode <- as.numeric(.Nifti$units[which(names(.Nifti$units) == unitName)])
    if (length(unitCode) == 0)
        unitCode <- 10
    else
        unitCode <- unitCode + 8
    
    origin <- (image$getOrigin() - 1) * abs(image$getVoxelDimensions())
    if (length(origin) > 3)
        origin <- origin[1:3]
    else if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    origin <- ifelse(origin < 0, rep(0,3), origin)
    origin[2:3] <- -origin[2:3]
    sformRows <- c(-fullVoxelDims[2], 0, 0, origin[1],
                    0, fullVoxelDims[3], 0, origin[2],
                    0, 0, fullVoxelDims[4], origin[3])
    
    connection <- fileFun(fileNames$headerFile, "w+b")
    
    writeBin(as.integer(348), connection, size=4)
    writeBin(raw(36), connection)
    writeBin(as.integer(fullDims), connection, size=2)
    writeBin(raw(14), connection)
    writeBin(as.integer(.Nifti$datatypes$codes[typeIndex]), connection, size=2)
    writeBin(as.integer(8*.Nifti$datatypes$sizes[typeIndex]), connection, size=2)
    writeBin(raw(2), connection)
    writeBin(fullVoxelDims, connection, size=4)
    
    # Voxel offset, data scaling slope and intercept
    writeBin(as.double(c(352,1,0)), connection, size=4)
    
    writeBin(raw(3), connection)
    writeBin(as.integer(unitCode), connection, size=1)
    writeBin(raw(24), connection)
    writeBin(charToRaw(description), connection, size=1)
    writeBin(raw(24+80-nchar(description)), connection)
    
    # NIfTI xform block: sform and qform codes are hardcoded to 2 here unless the image is 2D
    if (ndims == 2)
        writeBin(as.integer(c(0,0)), connection, size=2)
    else
        writeBin(as.integer(c(2,2)), connection, size=2)
    writeBin(c(0,1,0,origin), connection, size=4)
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
    
    writeBin(data, connection, size=.Nifti$datatypes$sizes[typeIndex])
    close(connection)
    
    if (image$isInternal())
        image$setSource(expandFileName(fileNames$fileStem))
}
