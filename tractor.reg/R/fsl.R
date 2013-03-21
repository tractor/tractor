registerImagesWithFlirt <- function (sourceFileName, targetFileName, targetMaskFileName = NULL, initAffine = NULL, types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, finalInterpolation = c("trilinear","nearestneighbour","sinc","spline"))
{
    if (!is.character(sourceFileName) || !is.character(targetFileName))
        report(OL$Error, "Source and target images must be specified by their filenames")
    
    finalInterpolation <- match.arg(finalInterpolation)
    
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
            sourceImage <- as(readImageFile(sourceFileName), "nifti")
            targetImage <- as(readImageFile(targetFileName), "nifti")
            initAffine <- convertAffine(initAffine, sourceImage, targetImage, "fsl")
        }
        write.table(initAffine, inputMatrixFile, row.names=FALSE, col.names=FALSE)
        initExpression <- paste(" -init", inputMatrixFile, sep=" ")
    }
    
    outputMatrixFile <- ensureFileSuffix(threadSafeTempFile(), "mat")
    logFile <- ensureFileSuffix(threadSafeTempFile(), "log")
    
    paramString <- paste("-in ", sourceFileName, " -ref ", targetFileName, initExpression, outputFileExpression, " -omat ", outputMatrixFile, " -bins 256 -cost corratio -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof ", affineDof, refweightExpression, " -interp ", finalInterpolation, " >", logFile, " 2>&1", sep="")
    execute("flirt", paramString, errorOnFail=TRUE)
    
    affine <- as.matrix(read.table(outputMatrixFile))
    transform <- Transformation$new(sourceImage=readImageFile(sourceFileName), targetImage=readImageFile(targetFileName), affineMatrices=list(affine), controlPointImages=list(), reverseControlPointImages=list(), method="flirt")
    
    result <- list(transform=transform, transformedImage=NULL, reverseTransformedImage=NULL)
    if (!estimateOnly)
        result$transformedImage <- readImageFile(outputFileName)
    
    # Tidy up
    if (!is.null(initAffine))
        unlink(inputMatrixFile)
    unlink(outputMatrixFile)
    unlink(logFile)
    
    return (result)
}
