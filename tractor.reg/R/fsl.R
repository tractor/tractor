registerImagesWithFlirt <- function (sourceFileName, targetFileName, targetMaskFileName = NULL, initAffine = NULL, affineDof = 12, estimateOnly = FALSE, finalInterpolation = 1, ...)
{
    if (!is.character(sourceFileName) || !is.character(targetFileName))
        report(OL$Error, "Source and target images must be specified by their filenames")
    if (!any(affineDof == c(6,7,9,12)))
        report(OL$Error, "The specified affine degrees of freedom is not valid")
    
    if (is.numeric(finalInterpolation))
        finalInterpolation <- c("nearestneighbour","trilinear","sinc","spline")[finalInterpolation]
    else
        finalInterpolation <- match.arg(finalInterpolation, c("nearestneighbour","trilinear","sinc","spline"))
    
    if (estimateOnly)
        outputFileExpression <- NULL
    else
    {
        outputFileName <- threadSafeTempFile()
        outputFileExpression <- paste(" -out", outputFileName, sep=" ")
    }
    
    if (is.null(targetMaskFileName))
        refweightExpression <- NULL
    else
        refweightExpression <- paste(" -refweight", targetMaskFileName, sep=" ")
    
    if (is.null(initAffine))
        initExpression <- NULL
    else
    {
        inputMatrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
        if (attr(initAffine, "affineType") == "niftyreg")
        {
            sourceImage <- readImageFile(sourceFileName, metadataOnly=TRUE, reorder=FALSE)
            targetImage <- readImageFile(targetFileName, metadataOnly=TRUE, reorder=FALSE)
            initAffine <- convertAffine(initAffine, sourceImage, targetImage, "fsl")
        }
        write.table(initAffine, inputMatrixFile, row.names=FALSE, col.names=FALSE)
        initExpression <- paste(" -init", inputMatrixFile, sep=" ")
    }
    
    outputMatrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
    logFile <- ensureFileSuffix(threadSafeTempFile(), "log")
    
    paramString <- paste("-in ", sourceFileName, " -ref ", targetFileName, initExpression, outputFileExpression, " -omat ", outputMatrixFile, " -bins 256 -cost corratio -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof ", affineDof, refweightExpression, " -interp ", finalInterpolation, " >", logFile, " 2>&1", sep="")
    
    startTime <- Sys.time()
    execute("flirt", paramString, errorOnFail=TRUE)
    endTime <- Sys.time()
    report(OL$Info, "FSL-FLIRT registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    
    affine <- as.matrix(read.table(outputMatrixFile))
    transform <- Transformation$new(sourceImage=readImageFile(sourceFileName,metadataOnly=TRUE,reorder=FALSE), targetImage=readImageFile(targetFileName,metadataOnly=TRUE,reorder=FALSE), affineMatrices=list(affine), controlPointImages=list(), reverseControlPointImages=list(), method="flirt")
    
    result <- list(transform=transform, transformedImage=NULL, reverseTransformedImage=NULL)
    if (!estimateOnly)
        result$transformedImage <- readImageFile(outputFileName, reorder=FALSE)
    
    # Tidy up
    if (!is.null(initAffine))
        unlink(inputMatrixFile)
    unlink(outputMatrixFile)
    unlink(logFile)
    
    return (result)
}
