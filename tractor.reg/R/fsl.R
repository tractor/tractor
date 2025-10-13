.imagePath <- function (image, resolve = FALSE, create = TRUE)
{
    if (is.character(image))
    {
        # Resolve symlinked file path if required
        if (resolve)
        {
            linkTarget <- Sys.readlink(identifyImageFileNames(image)$imageFile)
            if (!is.na(linkTarget) && linkTarget != "")
                return (expandFileName(linkTarget, dirname(image)))
        }
        return (image)
    }
    else if (is(image,"MriImage") && !image$isInternal())
        return (image$getSource())
    else if (create)
    {
        fileName <- threadSafeTempFile()
        RNifti::writeNifti(image, fileName)
        return (fileName)
    }
    else
        return (NULL)
}

registerImagesWithFlirt <- function (registration, sourceMask = NULL, targetMask = NULL, initAffine = NULL, affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    assert(registration$nTransforms() == 1, "The FSL-FLIRT interface is currently only for single registrations")
    sourceFileName <- .imagePath(registration$getSource())
    targetFileName <- .imagePath(registration$getTarget())
    
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
    if (!is.null(sourceMask))
        inweightExpression <- es("-inweight #{.imagePath(sourceMask)}")
    if (!is.null(targetMask))
        refweightExpression <- es("-refweight #{.imagePath(targetMask)}")
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
    registration$setTransforms(affine, "affine")
    
    if (!estimateOnly)
        registration$setTransformedImage(readImageFile(outputFileName, reorder=FALSE))
    
    # Tidy up
    if (!is.null(initAffine))
        unlink(inputMatrixFile)
    unlink(outputMatrixFile)
    unlink(logFile)
    
    invisible(registration)
}
