registerImagesWithFlirt <- function (sourceFileName, targetFileName, sourceMaskFileName = NULL, targetMaskFileName = NULL, initAffine = NULL, affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    if (!is.character(sourceFileName) || !is.character(targetFileName))
        report(OL$Error, "Source and target images must be specified by their filenames")
    if (!any(affineDof == c(6,7,9,12)))
        report(OL$Error, "The specified affine degrees of freedom is not valid")
    
    if (is.numeric(interpolation))
        interpolation <- c("nearestneighbour","trilinear","sinc","spline")[interpolation+1]
    else
        interpolation <- match.arg(interpolation, c("nearestneighbour","trilinear","sinc","spline"))
    
    if (estimateOnly)
        outputFileExpression <- ""
    else
    {
        outputFileName <- threadSafeTempFile()
        outputFileExpression <- es("-out #{outputFileName}")
    }
    
    if (is.null(sourceMaskFileName))
        inweightExpression <- ""
    else
        inweightExpression <- es("-inweight #{sourceMaskFileName}")
    if (is.null(targetMaskFileName))
        refweightExpression <- ""
    else
        refweightExpression <- es("-refweight #{targetMaskFileName}")
    
    if (is.null(initAffine))
        initExpression <- ""
    else
    {
        inputMatrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
        initAffine <- RNiftyReg:::convertAffine(initAffine, sourceFileName, targetFileName, "fsl")
        writeAffine(initAffine, inputMatrixFile, comments=FALSE)
        initExpression <- es("-init #{inputMatrixFile}")
    }
    
    outputMatrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
    logFile <- ensureFileSuffix(threadSafeTempFile(), "log")
    
    paramString <- es("-in #{sourceFileName} -ref #{targetFileName} #{initExpression} #{outputFileExpression} -omat #{outputMatrixFile} -bins 256 -cost corratio -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof #{affineDof} #{inweightExpression} #{refweightExpression} -interp #{interpolation} >#{logFile} 2>&1")
    
    startTime <- Sys.time()
    execute("flirt", paramString, errorOnFail=TRUE)
    endTime <- Sys.time()
    report(OL$Info, "FSL-FLIRT registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    
    affine <- readAffine(outputMatrixFile, sourceFileName, targetFileName, type="fsl")
    transform <- Transformation$new(readImageFile(sourceFileName,metadataOnly=TRUE,reorder=FALSE), readImageFile(targetFileName,metadataOnly=TRUE,reorder=FALSE), affineMatrices=list(affine), method="fsl")
    
    result <- list(transform=transform, transformedImage=NULL, reverseTransformedImage=NULL)
    if (!estimateOnly)
        result$transformedImage <- as(readNifti(outputFileName), "MriImage")
    
    # Tidy up
    if (!is.null(initAffine))
        unlink(inputMatrixFile)
    unlink(outputMatrixFile)
    unlink(logFile)
    
    return (result)
}
