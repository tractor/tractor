readMrtrix <- function (fileNames)
{
    if (!is.list(fileNames))
        fileNames <- identifyImageFileNames(fileNames)
    if (!file.exists(fileNames$headerFile))
        report(OL$Error, "File #{fileNames$headerFile} not found")
    
    # Find the end of the header
    match <- ore.search("\nEND\n", ore.file(fileNames$headerFile, binary=TRUE))
    assert(!is.null(match), "File #{fileNames$headerFile} does not seem to contain a well-formed MRtrix header")
    endOffset <- match$byteOffsets
    
    # The gzfile function can handle uncompressed files too
    connection <- gzfile(fileNames$headerFile, "rb")
    on.exit(close(connection))
    
    magic <- rawToChar(stripNul(readBin(connection, "raw", n=12)))
    assert(magic == "mrtrix image", "File #{fileNames$headerFile} does not appear to be a valid MRtrix image")
    
    fields <- rawToChar(stripNul(readBin(connection, "raw", n=endOffset-11)))
    match <- ore.search("^\\s*(\\w+): (.+)\\s*$", fields, all=TRUE)
    fields <- structure(as.list(match[,2]), names=match[,1])
    mergedFields <- list()
    for (fieldName in unique(names(fields)))
        mergedFields[[fieldName]] <- unlist(fields[names(fields) == fieldName], use.names=FALSE)
    
    pad <- function (x, minLength = 3L, value = 1)
    {
        length <- length(x)
        if (length >= minLength)
            return (x)
        else
            return (c(x, rep(value, minLength-length)))
    }
    
    # Extract and remove the specified field, splitting up elements
    getField <- function (name, split = "\\s*,\\s*", required = TRUE)
    {
        value <- mergedFields[[name]]
        if (required && is.null(value))
            report(OL$Error, "Required MRtrix header field \"#{name}\" is missing")
        else if (!is.null(value) && !is.null(split))
            value <- unlist(ore.split(split, value))
        mergedFields[[name]] <<- NULL
        return (value)
    }
    
    dims <- as.integer(getField("dim"))
    voxelDims <- as.numeric(getField("vox"))
    voxelDims[!is.finite(voxelDims)] <- 0
    
    layoutMatch <- ore.search("^(\\+|-)(\\d)$", getField("layout"), simplify=FALSE)
    signs <- ifelse(layoutMatch[,,1] == "+", 1, -1)
    axes <- as.integer(layoutMatch[,,2]) + 1L
    if (length(axes) > 3 && any(axes[4:length(axes)] != 4:length(axes)))
        report(OL$Error, "Images not stored in volume block order are not yet supported")
    
    # Find the inverse axis permutation and create an orientation string matching the data layout
    perm <- match(seq_along(axes), axes)
    orientation <- implode(c("I","P","L","","R","A","S")[pad(signs[perm]*axes[perm],3L,3L)[1:3]+4], sep="")
    
    # MRtrix stores the transform relative to the basic layout, and without
    # scaling for voxel dimensions, so we have to restore the scale factors and
    # convert from effective RAS to the actual orientation
    xform <- diag(c(pad(abs(voxelDims),3)[1:3], 1))
    if (!is.null(mergedFields$transform))
    {
        mrtrixTransform <- rbind(matrix(as.numeric(getField("transform")), nrow=3, ncol=4, byrow=TRUE), c(0,0,0,1))
        xform <- structure(mrtrixTransform %*% xform, imagedim=dims)
    }
    orientation(xform) <- orientation
    
    datatypeString <- as.character(getField("datatype"))
    if (datatypeString == "Bit" || datatypeString %~% "^C")
        report(OL$Error, "Bit and complex datatypes are not supported")
    datatypeMatch <- ore.search("^(U)?(Int|Float)(8|16|32|64)(LE|BE)?$", datatypeString)
    datatype <- list(code=0,
                     type=ifelse(datatypeMatch[,2]=="Int", "integer", "double"),
                     size=as.integer(datatypeMatch[,3]) / 8L,
                     isSigned=!is.na(datatypeMatch[,1]) || datatypeMatch[,2]=="Float")
    endianString <- ifelse(is.na(datatypeMatch[,4]), "", datatypeMatch[,4])
    endian <- switch(endianString, LE="little", BE="big", .Platform$endian)
    
    fileField <- getField("file")
    fileMatch <- ore.search("^\\s*(\\S+) (\\d+)\\s*$", fileField)
    
    scaling <- as.numeric(getField("scaling", required=FALSE))
    if (length(scaling) == 0)
        scaling <- c(0, 1)
    
    # Create a default header
    header <- niftiHeader()
    
    nDims <- length(dims)
    header$dim[seq_len(nDims+1)] <- c(nDims, dims)
    qform(header) <- structure(xform, code=2L)
    header$pixdim[seq_len(nDims)+1] <- voxelDims
    
    # Extract a diffusion gradient scheme into standard tags, if available
    tags <- NULL
    scheme <- getField("dw_scheme", required=FALSE)
    if (!is.null(scheme))
    {
        # Again, the diffusion directions are relative to real-world RAS space, so permute and flip as needed
        scheme <- matrix(as.numeric(scheme), ncol=4, byrow=TRUE)
        bVectors <- scheme[,pad(axes[perm],3L,3L)[1:3],drop=FALSE]
        flip <- which(signs < 0)
        if (any(flip <= 3))
            bVectors[,flip] <- -bVectors[,flip]
        tags <- list(bVectors=bVectors, bValues=scheme[,4])
    }
    
    # Fields are removed as they are used; remaining ones become tags
    tags <- c(tags, mergedFields)
    
    storage <- list(offset=as.integer(fileMatch[,2]), intercept=scaling[1], slope=scaling[2], datatype=datatype, endian=endian)
    invisible (list(image=NULL, header=header, storage=storage, tags=tags))
}
