#' Apply a transform
#' 
#' These functions transform between image spaces using a precalculated linear
#' or nonlinear transform. Transforms can be in `RNiftyReg` format, or
#' encapsulated in a `Registration` object.
#' 
#' These functions are interfaces to [RNiftyReg::applyTransform()], which
#' transforms both points and images. Note that image values are regridded in
#' the space of the transform's target image, and so interpolation is generally
#' required; the `interpolation` argument selects between different functions
#' for this. Parcellation images are labelled, so interpolated values do not
#' make sense in this case. Instead, each labelled region is extracted
#' individually and transformed with linear interpolation into the target
#' space; the highest-valued label is assigned to a voxel if its interpolated
#' value is above a user-defined threshold.
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
#' @param parcellation A list in the form created by [readParcellation()],
#'   representing a labelled image and associated metadata.
#' @param threshold The minimum interpolated value for a label to be retained
#'   in the transformed parcellation. Higher values will produce conservative
#'   parcellations, and lower values more inclusive ones.
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
    assert(all.equal(RNifti::xform(image), RNifti::xform(attr(transform,"source"))), "Specified image is not in the same space as the registration's source image")
    
    result <- applyTransform(transform, image, interpolation=.interpolationNameToCode(interpolation))
    return (as(result, "MriImage"))
}

# Parcellation images can be transformed using nearest neighbour interpolation, but this function gives more flexibility while still ensuring a sensible, nonoverlapping result
#' @rdname transformImage
#' @export
transformParcellation <- function (transform, parcellation, ..., threshold = 0.5)
{
    if (is(transform, "Registration"))
        transform <- transform$getTransforms(...)
    
    targetSpace <- as(attr(transform,"target"), "MriImage")
    
    # NB: "threshold * (1-.Machine$double.neg.eps)" should always evaluate (just) strictly less than "threshold"
    # This allows values at threshold to be kept without adding an extra evaluation every time below
    finalParcellation <- array(0L, dim=targetSpace$getDimensions())
    maxValues <- array(max(0, threshold * (1-.Machine$double.neg.eps)), dim=targetSpace$getDimensions())
    
    uniqueIndices <- sort(setdiff(unique(as.vector(parcellation$image$getData())), 0L))
    for (i in uniqueIndices)
    {
        currentImage <- parcellation$image$copy()$map(function(x) ifelse(x==i,1,0))
        transformedImage <- transformImage(transform, currentImage, interpolation=1)
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

#' Plot a transform's deformation field
#' 
#' This function visualises a transform in terms of its deformation field in a
#' specified plane. It shows a slice in source space, overlaid with a field of
#' marks showing the locations of target voxel centres. The colours of the
#' marks correspond to the local Jacobian determinant, indicating whether there
#' is an expansion or contraction in the area. The Jacobian will be constant
#' for linear transforms.
#' 
#' @inheritParams transformImage
#' @param x,y,z Coordinate elements (after standard image reordering). Exactly
#'   one of these should be specified, identifying the plane in target space
#'   that you want to visualise.
#' @param sourceImage The full source image, if it cannot be derived from the
#'   `transform`. This will be used for the base layer of the plot.
#' 
#' @note There will generally not be a plane in source space that matches the
#'   specified target space plane exactly. The source plane used is the slice
#'   in the same orientation that is closest on average to the target space
#'   points, but the overlay is an approximation only, and in some cases may
#'   not be a very good one.
#' @export
plotTransform <- function (transform, ..., x = NA, y = NA, z = NA, sourceImage = NULL)
{
    reorderPoints <- function (points, image)
    {
        rotationMatrix <- RNifti::rotation(image$getXform())
        orientation <- apply(abs(rotationMatrix) > 0.5, 2, which)
        orientation <- orientation * sign(rotationMatrix[cbind(orientation,1:3)]) * c(-1,1,1)
        dimPermutation <- match(1:3, abs(orientation))
        points <- points[,dimPermutation,drop=FALSE]
        
        ordering <- orientation[dimPermutation]
        if (any(ordering < 0))
        {
            dims <- image$getDimensions()[1:3]
            for (i in which(ordering < 0))
                points[,i] <- dims[i] - points[,i] + 1
        }
        
        return (points)
    }
    
    if (is(transform, "Registration"))
    {
        # The source attribute of the transform does not contain data so isn't sufficient
        if (is.null(sourceImage))
            sourceImage <- as(transform$getSource(), "MriImage")
        transform <- transform$getTransforms(...)
    }
    else
        sourceImage <- as(sourceImage %||% attr(transform, "source"), "MriImage")
    assert(!sourceImage$isEmpty(), "A full source image is required")
    
    loc <- c(x, y, z)
    throughPlaneAxis <- which(!is.na(loc))
    assert(length(throughPlaneAxis) == 1L, "Exactly one element of the location should be specified")
    inPlaneAxes <- setdiff(1:3, throughPlaneAxis)
    
    # Calculate the deformation field
    deformationField <- deformationField(transform, jacobian=TRUE)
    
    # Find the requested slice of the Jacobian map and deformation field
    jacobian <- reorderMriImage(as(jacobian(deformationField),"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    field <- reorderMriImage(as(deformationField,"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    
    # Convert the field to voxel positions and find the closest plane (on average) in source space
    fieldDims <- dim(field)
    dim(field) <- c(prod(fieldDims[1:2]), fieldDims[3])
    fieldVoxels <- reorderPoints(worldToVoxel(field,sourceImage$getMetadata()), sourceImage$getMetadata())
    sourceLoc <- rep(NA, 3)
    sourceLoc[throughPlaneAxis] <- round(mean(fieldVoxels[,throughPlaneAxis], na.rm=TRUE))
    fieldVoxels <- fieldVoxels[,inPlaneAxes]
    
    # Use the 2.5% trimmed range of the whole Jacobian map to create a colour scale (centred at 1)
    jacobianTrimmedRange <- quantile(abs(as.array(jacobian(deformationField))), c(0.025,0.975), na.rm=TRUE) - 1
    vizLimit <- max(abs(jacobianTrimmedRange))
    report(OL$Info, "Jacobian colour range is from #{1-vizLimit} to #{1+vizLimit}", round=3)
    colourIndices <- 1 + round(99 * ((abs(jacobian)-1) + vizLimit) / (2*vizLimit))
    colourIndices[colourIndices < 1] <- 1
    colourIndices[colourIndices > 100] <- 100
    colours <- paste(getColourScale(4)$colours, "60", sep="")
    
    # Create the visualisation
    createSliceGraphic(sourceImage, sourceLoc[1], sourceLoc[2], sourceLoc[3])
    width <- sourceImage$getDimensions()[inPlaneAxes] - 1
    points((fieldVoxels[,1]-1)/width[1], (fieldVoxels[,2]-1)/width[2], pch=3, col=colours[colourIndices])
}
