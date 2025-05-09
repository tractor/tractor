#' Apply a transform
#' 
#' These functions transform between image spaces using a precalculated linear
#' or nonlinear transform. Transforms can be in `RNiftyReg` format, or
#' encapsulated in a `Registration` object.
#' 
#' @param transform An `RNiftyReg` transform (affine matrix or control point
#'   image), or a [Registration] object.
#' @param image An image to transform into the target space of the transform.
#'   If `NULL`, and `transform` is a `Registration` object, the source image
#'   for the registration will be used by default. (It must be a complete
#'   image for this to work, not just a header.)
#' @param ... Additional arguments to the `getTransforms()` method of the
#'   `Registration` class, if `transform` is of that class. This allows reverse
#'   or half transforms to be applied.
#' @param interpolation Integer image interpolation degree or name: 0
#'   (nearestneighbour), 1 (trilinear) or 3 (cubicspline).
#' @param points A numeric vector specifying a single point to transform
#'   between spaces, or a matrix with one point per row.
#' @param voxel Boolean value, indicating whether the specified `points` are in
#'   voxel terms (`TRUE`) or world terms (`FALSE`). The latter is only
#'   supported when `transform` is a full registration, because source and
#'   target space information is required.
#' @param nearest Boolean value: should the resulting points be rounded to the
#'   nearest integer? This can save a little computation for nonlinear
#'   transforms, but should be `FALSE` if subvoxel precision is needed.
#' @return `transformImage` returns a transformed `MriImage` object.
#'   `transformPoints` returns a transformed numeric vector or matrix of
#'   points.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
transformImage <- function (transform, image = NULL, ..., interpolation = 1)
{
    if (is(transform, "Registration"))
    {
        image <- image %||% transform$getSource()
        transform <- transform$getTransforms(...)
    }
    
    assert(!is(image,"MriImage") || !image$isRgb(), "RGB images cannot be transformed yet")
    
    result <- applyTransform(transform, image, interpolation=.interpolationNameToCode(interpolation))
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

#' @rdname transformImage
#' @export
transformPoints <- function (transform, points, ..., voxel = TRUE, nearest = FALSE)
{
    if (is(transform, "Registration"))
        transform <- transform$getTransforms(...)
    
    if (!voxel)
        points <- RNifti::worldToVoxel(points, attr(transform,"source"))
    
    newPoints <- applyTransform(transform, points, nearest=nearest)
    
    if (!voxel)
        newPoints <- RNifti::voxelToWorld(newPoints, attr(transform,"target"))
    
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
