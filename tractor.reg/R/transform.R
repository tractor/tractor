transformImage <- function (transform, newImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, half = FALSE, interpolation = 1)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    if (is.null(newImage))
        newImage <- transform$getSourceImage(index, reverse)
    
    assert(!is(newImage,"MriImage") || !newImage$isRgb(), "RGB images cannot be transformed yet")
    
    xfm <- transform$getTransformObjects(index, reverse, preferAffine, half)
    result <- applyTransform(xfm, newImage, interpolation=.interpolationNameToCode(interpolation))
    
    return (as(result, "MriImage"))
}

# Parcellation images can be transformed using nearest neighbour interpolation, but this function gives more flexibility while still ensuring a sensible, nonoverlapping result
transformParcellation <- function (transform, parcellation, threshold = 0.5, index = 1, preferAffine = FALSE, reverse = FALSE, half = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    targetSpace <- transform$getTargetImage(index, reverse)
    
    # NB: "threshold * (1-.Machine$double.neg.eps)" should always evaluate (just) strictly less than "threshold"
    # This allows values at threshold to be kept without adding an extra evaluation every time below
    finalParcellation <- array(0L, dim=targetSpace$getDimensions())
    maxValues <- array(max(0, threshold * (1-.Machine$double.neg.eps)), dim=targetSpace$getDimensions())
    
    uniqueIndices <- sort(setdiff(unique(as.vector(parcellation$image$getData())), 0L))
    for (i in uniqueIndices)
    {
        currentImage <- parcellation$image$copy()$map(function(x) ifelse(x==i,1,0))
        transformedImage <- transformImage(transform, currentImage, index=index, preferAffine=preferAffine, reverse=reverse, half=half, interpolation=1)
        toUpdate <- which(transformedImage$getData() > maxValues)
        if (length(toUpdate) > 0)
        {
            finalParcellation[toUpdate] <- i
            maxValues[toUpdate] <- transformedImage[toUpdate]
        }
        else
            report(OL$Warning, "Region with index #{i} is unrepresented in the transformed parcellation")
    }
    
    finalImage <- asMriImage(finalParcellation, targetSpace)
    return (list(image=finalImage, regions=parcellation$regions))
}

transformPoints <- function (transform, points, voxel = TRUE, index = 1, preferAffine = FALSE, reverse = FALSE, half = FALSE, nearest = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    if (!voxel)
        points <- worldToVoxel(points, transform$getSourceImage(index,reverse))
    
    xfm <- transform$getTransformObjects(index, reverse, preferAffine, half)
    newPoints <- applyTransform(xfm, points, nearest=nearest)
    
    if (!voxel)
        newPoints <- voxelToWorld(newPoints, transform$getTargetImage(index,reverse))
    
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

transformVoxelToWorld <- function (points, image, simple = FALSE, ...)
{
    RNiftyReg::voxelToWorld(points, image, simple=simple, ...)
}

transformWorldToVoxel <- function (points, image, simple = FALSE, ...)
{
    RNiftyReg::worldToVoxel(points, image, simple=simple, ...)
}
