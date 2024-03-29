readNifti <- function (fileNames, metadataOnly = FALSE, volumes = NULL)
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
    else if (metadataOnly)
        result$header <- niftiHeader(fileNames$headerFile)
    else
        result$image <- RNifti::readNifti(fileNames$headerFile, volumes=volumes)
    
    invisible (result)
}

writeNifti <- function (image, fileNames, datatype = "fit")
{
    if (datatype == "fit")
        datatype <- chooseDataTypeForImage(image, "Nifti")
    
    image <- asNifti(image, internal=TRUE)
    image$descrip <- "TractoR NIfTI writer v3.4.0"
    
    RNifti::writeNifti(image, fileNames$headerFile, datatype=datatype)
}
