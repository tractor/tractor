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
        points <- transformVoxelToWorld(points, sourceImage, simple=TRUE)
        
    if (type == "affine")
    {
        affine <- transform$getAffineMatrix(index)
        if (reverse)
            affine <- invertAffine(affine)
        
        newPoints <- transformWithAffine(points, affine, as(source,"nifti"), as(target,"nifti"))
        
        if (nearest)
            newPoints <- round(newPoints)
    }
    else
        newPoints <- transformWithControlPoints(points, as(controlPoints,"nifti"), as(source,"nifti"), as(target,"nifti"), nearest=nearest)
    
    if (!voxel)
        newPoints <- transformWorldToVoxel(newPoints, targetImage, simple=TRUE)
    
    return (newPoints)
}

translatePoints <- function (points, offset)
{
    if (is.vector(points))
        return (points + offset)
    else if (is.matrix(points))
        return (t(apply(points, 1, "+", offset)))
}

.applyTransform <- function (x, m)
{
    if (is.null(x))
        return (m)
    else if (is.matrix(x))
    {
        x <- cbind(x,1)
        len <- ncol(x)
        result <- m %*% t(x)
        return (t(result[1:(len-1),]))
    }
    else if (is.vector(x))
    {
        x <- c(x,1)
        len <- length(x)
        result <- m %*% x
        return (drop(result[1:(len-1),]))
    }
}

transformWithTranslation <- function (x, ...)
{
    translation <- c(..., 1)
    len <- length(translation)
    
    m <- diag(len)
    m[,len] <- translation
    
    return (.applyTransform(x, m))
}

transformWithScaling <- function (x, ...)
{
    scaling <- c(..., 1)
    m <- diag(scaling)
    
    return (.applyTransform(x, m))
}

transformRVoxelToFslVoxel <- function (x)
{
    if (is.null(x))
        len <- 3
    else if (is.matrix(x))
        len <- ncol(x)
    else if (is.vector(x))
        len <- length(x)
    return (transformWithTranslation(x, rep(-1,len)))
}

transformFslVoxelToRVoxel <- function (x)
{
    if (is.null(x))
        len <- 3
    else if (is.matrix(x))
        len <- ncol(x)
    else if (is.vector(x))
        len <- length(x)
    return (transformWithTranslation(x, rep(1,len)))
}

transformFslVoxelToWorld <- function (x, metadata, useOrigin = FALSE)
{
    if (useOrigin)
    {
        if (is.null(x))
        {
            m1 <- transformFslVoxelToRVoxel(NULL)
            m2 <- transformWithTranslation(NULL, -metadata$getOrigin())
            m3 <- transformWithScaling(NULL, metadata$getVoxelDimensions())
            return (m3 %*% m2 %*% m1)
        }
        else
        {
            rVoxel <- transformFslVoxelToRVoxel(x)
            zeroOrigin <- transformWithTranslation(rVoxel, -metadata$getOrigin())
            worldLoc <- transformWithScaling(zeroOrigin, metadata$getVoxelDimensions())
            return (worldLoc)
        }
    }
    else
        return (transformWithScaling(x, abs(metadata$getVoxelDimensions())))
}

transformWorldToFslVoxel <- function (x, metadata, useOrigin = FALSE)
{
    if (useOrigin)
    {
        if (is.null(x))
        {
            m1 <- transformWithScaling(NULL, 1/metadata$getVoxelDimensions())
            m2 <- transformWithTranslation(NULL, metadata$getOrigin())
            m3 <- transformRVoxelToFslVoxel(NULL)
            return (m3 %*% m2 %*% m1)
        }
        else
        {
            zeroOrigin <- transformWithScaling(x, 1/metadata$getVoxelDimensions())
            rVoxel <- transformWithTranslation(zeroOrigin, metadata$getOrigin())
            fslVoxel <- transformRVoxelToFslVoxel(rVoxel)
            return (fslVoxel)
        }
    }
    else
        return (transformWithScaling(x, abs(1/metadata$getVoxelDimensions())))
}

transformRVoxelToWorld <- function (x, metadata, useOrigin = FALSE)
{
    if (is.null(x))
    {
        m1 <- transformRVoxelToFslVoxel(NULL)
        m2 <- transformFslVoxelToWorld(NULL, metadata, useOrigin=useOrigin)
        return (m2 %*% m1)
    }
    else
    {
        fslVoxel <- transformRVoxelToFslVoxel(x)
        worldLoc <- transformFslVoxelToWorld(fslVoxel, metadata, useOrigin=useOrigin)
        return (worldLoc)
    }
}

transformWorldToRVoxel <- function (x, metadata, useOrigin = FALSE)
{
    if (is.null(x))
    {
        m1 <- transformWorldToFslVoxel(NULL, metadata, useOrigin=useOrigin)
        m2 <- transformFslVoxelToRVoxel(NULL)
        return (m2 %*% m1)
    }
    else
    {
        fslVoxel <- transformWorldToFslVoxel(x, metadata, useOrigin=useOrigin)
        rVoxel <- transformFslVoxelToRVoxel(fslVoxel)
        return (rVoxel)
    }
}
