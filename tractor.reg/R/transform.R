transformVoxelToWorld <- function (points, image, simple = FALSE, ...)
{
    return (RNiftyReg::transformVoxelToWorld(points, as(image,"nifti"), simple, ...))
}

transformWorldToVoxel <- function (points, image, simple = FALSE, ...)
{
    return (RNiftyReg::transformWorldToVoxel(points, as(image,"nifti"), simple, ...))
}

transformImage <- function (transform, newImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, finalInterpolation = 1)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    options <- list(nLevels=0, verbose=FALSE)
    
    availableTypes <- transform$getTypes()
    if (preferAffine && ("affine" %in% availableTypes))
        type <- "affine"
    else if (reverse && ("reverse-nonlinear" %in% availableTypes))
    {
        type <- "nonlinear"
        options$initControl <- transform$getReverseControlPointImage(index)
    }
    else if (!reverse && ("nonlinear" %in% availableTypes))
    {
        type <- "nonlinear"
        options$initControl <- transform$getControlPointImage(index)
    }
    else if ("affine" %in% availableTypes)
        type <- "affine"
    else
        report(OL$Error, "The specified Transformation object does not contain the necessary information")
    
    if (!is.null(newImage))
        sourceImage <- newImage
    else if (reverse)
        sourceImage <- transform$getTargetImage()
    else
        sourceImage <- transform$getSourceImage()
    
    if (reverse)
        targetImage <- transform$getSourceImage()
    else
        targetImage <- transform$getTargetImage()
    
    if (type == "affine")
    {
        initAffine <- transform$getAffineMatrix(index)
        if (reverse)
            initAffine <- invertAffine(initAffine)
        options$scope <- "affine"
        
        result <- registerImagesWithNiftyreg(sourceImage, targetImage, initAffine=initAffine, types="affine", estimateOnly=FALSE, finalInterpolation=finalInterpolation, linearOptions=options)
    }
    else
        result <- registerImagesWithNiftyreg(sourceImage, targetImage, types="nonlinear", estimateOnly=FALSE, finalInterpolation=finalInterpolation, nonlinearOptions=options)
    
    return (result)
}

transformPoints <- function (transform, points, voxel = TRUE, index = 1, preferAffine = FALSE, reverse = FALSE, nearest = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    availableTypes <- transform$getTypes()
    if (preferAffine && ("affine" %in% availableTypes))
        type <- "affine"
    else if (reverse && ("reverse-nonlinear" %in% availableTypes))
    {
        type <- "nonlinear"
        controlPoints <- transform$getReverseControlPointImage(index)
    }
    else if (!reverse && ("nonlinear" %in% availableTypes))
    {
        type <- "nonlinear"
        controlPoints <- transform$getControlPointImage(index)
    }
    else if ("affine" %in% availableTypes)
        type <- "affine"
    else
        report(OL$Error, "The specified Transformation object does not contain the necessary information")
    
    if (reverse)
    {
        sourceImage <- transform$getTargetImage()
        targetImage <- transform$getSourceImage()
    }
    else
    {
        sourceImage <- transform$getSourceImage()
        targetImage <- transform$getTargetImage()
    }
    
    if (!voxel)
        points <- transformWorldToVoxel(points, sourceImage)
        
    if (type == "affine")
    {
        affine <- transform$getAffineMatrix(index)
        if (reverse)
            affine <- invertAffine(affine)
        
        newPoints <- transformWithAffine(points, affine, as(sourceImage,"nifti"), as(targetImage,"nifti"))
        
        if (nearest)
            newPoints <- round(newPoints)
    }
    else
        newPoints <- transformWithControlPoints(points, as(controlPoints,"nifti"), as(sourceImage,"nifti"), as(targetImage,"nifti"), nearest=nearest)
    
    if (!voxel)
        newPoints <- transformVoxelToWorld(newPoints, targetImage)
    
    return (newPoints)
}

translatePoints <- function (points, offset)
{
    if (is.vector(points))
        return (points + offset)
    else if (is.matrix(points))
        return (t(apply(points, 1, "+", offset)))
}
