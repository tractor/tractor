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
    
    return (result$transformedImage)
}

# Parcellation images can be transformed using nearest neighbour interpolation, but this function gives more flexibility while still ensuring a sensible, nonoverlapping result
transformParcellation <- function (transform, parcellationImage, threshold = 0.5, index = 1, preferAffine = FALSE, reverse = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    if (reverse)
        targetSpace <- transform$getSourceImage()
    else
        targetSpace <- transform$getTargetImage()
    
    # NB: "threshold - .Machine$double.eps" should always evaluate (just) strictly less than "threshold"
    # This allows values at threshold to be kept without adding an extra evaluation every time below
    finalParcellation <- array(0L, dim=targetSpace$getDimensions())
    maxValues <- array(max(0, threshold - .Machine$double.eps), dim=targetSpace$getDimensions())
    
    uniqueIndices <- setdiff(unique(as.vector(parcellationImage$getData())), 0L)
    for (i in uniqueIndices)
    {
        currentImage <- newMriImageWithSimpleFunction(parcellationImage, function(x) ifelse(x==i,1,0))
        transformedImage <- transformImage(transform, currentImage, index=index, preferAffine=preferAffine, reverse=reverse, finalInterpolation=1)
        toUpdate <- which(transformedImage$getData() > maxValues)
        if (length(toUpdate) > 0)
        {
            finalParcellation[toUpdate] <- i
            maxValues[toUpdate] <- transformedImage[toUpdate]
        }
        else
            report(OL$Warning, "Region with index #{i} is unrepresented in the transformed parcellation")
    }
    
    finalImage <- newMriImageWithData(finalParcellation, targetSpace)
    return (finalImage)
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
        
        newPoints <- transformWithAffine(points, affine, sourceImage, targetImage)
        
        if (nearest)
            newPoints <- round(newPoints)
    }
    else
        newPoints <- transformWithControlPoints(points, controlPoints, sourceImage, targetImage, nearest=nearest)
    
    if (!voxel)
        newPoints <- transformVoxelToWorld(newPoints, targetImage)
    
    return (newPoints)
}

translatePoints <- function (points, offset)
{
    if (is.matrix(points))
        return (t(apply(points, 1, "+", offset)))
    else if (is.numeric(points))
        return (points + offset)
    else
        report(OL$Error, "Points must be specified as a numeric vector or matrix")
}
