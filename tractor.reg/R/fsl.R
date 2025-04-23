registerImagesWithFlirt <- function (transform, sourceMaskFileName = NULL, targetMaskFileName = NULL, initAffine = NULL, affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    sourceFileName <- transform$getSourceImagePath()
    targetFileName <- transform$getTargetImagePath()
    
    if (!any(affineDof == c(6,7,9,12)))
        report(OL$Error, "The specified affine degrees of freedom is not valid")
    
    if (is.numeric(interpolation))
        interpolation <- c("nearestneighbour","trilinear","sinc","spline")[interpolation+1]
    else
        interpolation <- match.arg(interpolation, c("nearestneighbour","trilinear","sinc","spline"))
    
    outputFileExpression <- inweightExpression <- refweightExpression <- initExpression <- ""
    if (!estimateOnly)
    {
        outputFileName <- threadSafeTempFile()
        outputFileExpression <- es("-out #{outputFileName}")
    }
    if (!is.null(sourceMaskFileName))
        inweightExpression <- es("-inweight #{sourceMaskFileName}")
    if (!is.null(targetMaskFileName))
        refweightExpression <- es("-refweight #{targetMaskFileName}")
    if (!is.null(initAffine))
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
    transform$updateFromObjects(affineMatrices=list(affine), method="fsl")
    
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
