MriImage <- getRefClass("MriImage")

Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
    initialize = function (sourceImage = MriImage$new(), targetImage = MriImage$new(), affineMatrices = NULL, controlPointImages = NULL, reverseControlPointImages = NULL, method = c("niftyreg","fsl"), ...)
    {
        if (!is.list(affineMatrices))
            affineMatrices <- list(affineMatrices)
        affineMatrices <- lapply(affineMatrices, function(x) {
            # For backwards compatibility, handle FSL-type affines
            if (isTRUE(attr(x,"affineType") == "fsl"))
                x <- RNiftyReg:::convertAffine(x, as(sourceImage,"niftiImage"), as(targetImage,"niftiImage"), "niftyreg")
            mostattributes(x) <- NULL
            return (x)
        })
        
        if (!is.list(controlPointImages))
            controlPointImages <- list(controlPointImages)
        controlPointImages <- lapply(controlPointImages, function(x) { if (is.null(x)) x else as(x,"MriImage") })
        
        if (!is.list(reverseControlPointImages))
            reverseControlPointImages <- list(reverseControlPointImages)
        reverseControlPointImages <- lapply(reverseControlPointImages, function(x) { if (is.null(x)) x else as(x,"MriImage") })
        
        initFields(sourceImage=sourceImage, targetImage=targetImage, affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method=match.arg(method))
    },
    
    getAffineMatrices = function (i = NULL)
    {
        if (is.null(i))
            return (affineMatrices)
        else if (length(i) == 1 && i <= length(affineMatrices))
            return (affineMatrices[[i]])
        else if (length(i) > 1)
            return (affineMatrices[i])
        else
            return (NULL)
    },
    
    getControlPointImages = function (i = NULL)
    {
        if (is.null(i))
            return (controlPointImages)
        else if (length(i) == 1 && i <= length(controlPointImages))
            return (controlPointImages[[i]])
        else if (length(i) > 1)
            return (controlPointImages[i])
        else
            return (NULL)
    },
    
    getMethod = function () { return (method) },
    
    getReverseControlPointImages = function (i = NULL)
    {
        if (is.null(i))
            return (reverseControlPointImages)
        else if (length(i) == 1 && i <= length(reverseControlPointImages))
            return (reverseControlPointImages[[i]])
        else if (length(i) > 1)
            return (reverseControlPointImages[i])
        else
            return (NULL)
    },
    
    getSourceImage = function (i = NULL, reverse = FALSE)
    {
        if (reverse)
            return (targetImage)
        else if (is.null(i) || .self$nRegistrations() == 1)
            return (sourceImage)
        else
            return (extractMriImage(sourceImage, ndim(sourceImage), i))
    },
    
    getTargetImage = function (i = NULL, reverse = FALSE)
    {
        if (reverse)
            return (.self$getSourceImage(i))
        else
            return (sourceImage)
    },
    
    getTransformObject = function (i = 1, reverse = FALSE, preferAffine = FALSE, errorIfMissing = TRUE)
    {
        source <- as(.self$getSourceImage(i,reverse), "niftiImage")
        target <- as(.self$getTargetImage(i,reverse), "niftiImage")
        object <- NULL
        
        if (reverse)
        {
            # NB: for the first case, we initially pass the images in reverse order
            if (!is.null(.self$getAffineMatrices(i)) && (is.null(.self$getReverseControlPointImages(i)) || preferAffine))
                object <- invertAffine(asAffine(.self$getAffineMatrices(i), target, source))
            else if (!is.null(.self$getReverseControlPointImages(i)))
                object <- structure(as(.self$getReverseControlPointImages(i),"niftiImage"), source=source, target=target)
            else if (errorIfMissing)
                report(OL$Error, "No suitable reverse transform is available for index #{i}")
        }
        else
        {
            if (!is.null(.self$getAffineMatrices(i)) && (is.null(.self$getControlPointImages(i)) || preferAffine))
                object <- asAffine(.self$getAffineMatrices(i), source, target)
            else if (!is.null(.self$getControlPointImages(i)))
                object <- structure(as(.self$getControlPointImages(i),"niftiImage"), source=source, target=target)
            else if (errorIfMissing)
                report(OL$Error, "No suitable forward transform is available for index #{i}")
        }
        
        return (object)
    },
    
    getTypes = function ()
    {
        transformTypeNames <- c("affine", "nonlinear", "reverse-nonlinear")
        availability <- sapply(list(affineMatrices,controlPointImages,reverseControlPointImages), function(x) sum(!sapply(x,is.null))) > 0
        return (transformTypeNames[availability])
    },
    
    nRegistrations = function ()
    {
        if (ndim(sourceImage) == ndim(targetImage))
            return (1L)
        else
            return (length(dim(targetImage)[ndim(targetImage)]))
    },
    
    summarise = function ()
    {
        sourceSummary <- sourceImage$summarise()
        targetSummary <- targetImage$summarise()
        
        values <- c(sourceSummary$values[match(c("Image dimensions","Voxel dimensions"), sourceSummary$labels)], targetSummary$values[match(c("Image dimensions","Voxel dimensions"), targetSummary$labels)])
        values <- c(.self$getMethod(), implode(.self$getTypes(),", "), values)
        names(values) <- c("Registration method", "Stored transformations", "Source image dimensions", "Source voxel dimensions", "Target image dimensions", "Target voxel dimensions")
        
        return (values)
    }
))

plot.Transformation <- function (x, y = NULL, xLoc = NA, yLoc = NA, zLoc = NA, sourceImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, ...)
{
    reorderPoints <- function (points, image)
    {
        xform <- image$getStoredXformMatrix()
        if (!is.matrix(xform) || !equivalent(dim(xform),c(4,4)))
            return (points)
        
        orientation <- tractor.base:::xformToOrientation(xform, string=FALSE)
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
    
    loc <- c(xLoc, yLoc, zLoc)
    throughPlaneAxis <- which(!is.na(loc))
    if (length(throughPlaneAxis) != 1)
        report(OL$Error, "Exactly one element of the location should be specified")
    inPlaneAxes <- setdiff(1:3, throughPlaneAxis)
    
    if (is.null(sourceImage))
    {
        sourceImage <- x$getSourceImage(index, reverse)
        
        if (sourceImage$isEmpty())
        {
            if (sourceImage$isInternal())
                report(OL$Error, "Original image source is unknown - a source image must be provided")
            sourceImage <- readImageFile(sourceImage$getSource())
        }
        else if (!sourceImage$isReordered())
            sourceImage <- reorderMriImage(sourceImage)
    }
    
    # Calculate the deformation field
    deformationField <- RNiftyReg::deformationField(x$getTransformObject(index,reverse,preferAffine), jacobian=TRUE)
    
    # Find the requested slice of the Jacobian map and deformation field
    jacobian <- reorderMriImage(as(jacobian(deformationField),"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    field <- reorderMriImage(as(deformationField,"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    
    # Remove the last row and column from the data, because NiftyReg seems to calculate it wrongly
    # fieldDims <- dim(field)
    # jacobian <- jacobian[1:(fieldDims[1]-1),1:(fieldDims[2]-1)]
    # field <- field[1:(fieldDims[1]-1),1:(fieldDims[2]-1),,]
    
    # Convert the field to voxel positions and find the closest plane (on average) in source space
    fieldDims <- dim(field)
    dim(field) <- c(prod(fieldDims[1:2]), fieldDims[3])
    fieldVoxels <- reorderPoints(worldToVoxel(field,sourceImage$getMetadata()), sourceImage$getMetadata())
    sourceLoc <- rep(NA, 3)
    sourceLoc[throughPlaneAxis] <- round(mean(fieldVoxels[,throughPlaneAxis], na.rm=TRUE))
    fieldVoxels <- fieldVoxels[,inPlaneAxes]
    
    # Use the 2.5% trimmed range of the whole Jacobian map to create a colour scale (centred at 1)
    jacobianTrimmedRange <- quantile(abs(deformationField$jacobian@.Data), c(0.025,0.975), na.rm=TRUE) - 1
    vizLimit <- max(abs(jacobianTrimmedRange))
    colourIndices <- 1 + round(99 * ((abs(jacobian)-1) + vizLimit) / (2*vizLimit))
    colourIndices[colourIndices < 1] <- 1
    colourIndices[colourIndices > 100] <- 100
    colours <- paste(getColourScale(4)$colours, "60", sep="")
    
    # Create the visualisation
    createSliceGraphic(sourceImage, sourceLoc[1], sourceLoc[2], sourceLoc[3])
    width <- sourceImage$getDimensions()[inPlaneAxes] - 1
    points((fieldVoxels[,1]-1)/width[1], (fieldVoxels[,2]-1)/width[2], pch=3, col=colours[colourIndices])
}

registerImages <- function (sourceImage, targetImage, sourceMask = NULL, targetMask = NULL, method = getOption("tractorRegistrationMethod"), types = "affine", affineDof = 12, estimateOnly = FALSE, interpolation = 1, cache = c("auto","read","write","ignore"), file = NULL, ...)
{
    if (is.null(method))
        method <- "niftyreg"
    else
        method <- match.arg(method, c("niftyreg","fsl"))
    types <- match.arg(types, c("affine","nonlinear","reverse-nonlinear"), several.ok=TRUE)
    cache <- match.arg(cache)
    
    transform <- NULL
    fileHit <- FALSE
    cacheHit <- FALSE
    
    if (!is.null(file) && file.exists(file))
    {
        transform <- deserialiseReferenceObject(file)
        if (all(types %in% transform$getTypes()))
            fileHit <- TRUE
        else
            transform <- NULL
    }
    
    if (cache %in% c("auto","read"))
    {
        transform <- checkTransformationCache(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), method, types)
        if (!is.null(transform))
            cacheHit <- TRUE
    }
    
    if (!is.null(transform))
    {
        if (estimateOnly)
            result <- list(transform=transform, transformedImage=NULL, reverseTransformedImage=NULL)
        else
            result <- list(transform=transform, transformedImage=transformImage(transform,interpolation=interpolation,...))
    }
    else if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(getImageAsObject(sourceImage,reorder=FALSE), getImageAsObject(targetImage,reorder=FALSE), sourceMask=getImageAsObject(sourceMask,allowNull=TRUE,reorder=FALSE), targetMask=getImageAsObject(targetMask,allowNull=TRUE,reorder=FALSE), types=types, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    else if (method == "fsl")
    {
        if (any(c("nonlinear","reverse-nonlinear") %in% types))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        
        result <- registerImagesWithFlirt(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), sourceMaskFileName=getImageAsFileName(sourceMask,allowNull=TRUE), targetMaskFileName=getImageAsFileName(targetMask,allowNull=TRUE), affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    }
    
    if (cache == "write" || (cache == "auto" && !cacheHit))
        updateTransformationCache(result$transform, force=TRUE)
    
    if (!is.null(file) && !fileHit)
        result$transform$serialise(file)
    
    return (result)
}

resampleImage <- function (image, voxelDims = NULL, imageDims = NULL, interpolation = 1)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        report(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    scales <- voxelDims / image$getVoxelDimensions()
    
    sourceNifti <- as(image, "niftiImage")
    xfm <- buildAffine(scales=scales, source=sourceNifti)
    result <- applyTransform(xfm, sourceNifti, interpolation=interpolation)
    
    return (as(result, "MriImage"))
}

identityTransformation <- function (sourceImage, targetImage)
{
    xfm <- buildAffine(source=sourceImage, target=targetImage)
    transform <- Transformation$new(sourceImage, targetImage, affineMatrices=list(xfm), method="niftyreg")
    return (transform)
}

invertTransformation <- function (transform, quiet = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    if (transform$nRegistrations() > 1)
        report(OL$Error, "Transformations covering more than one registration cannot be directly inverted")
    
    affineMatrices <- controlPointImages <- reverseControlPointImages <- list()
    
    availableTypes <- transform$getTypes()
    if (all(c("nonlinear","reverse-nonlinear") %in% availableTypes))
    {
        controlPointImages <- transform$getReverseControlPointImages()
        reverseControlPointImages <- transform$getControlPointImages()
    }
    else if (!quiet && "nonlinear" %in% availableTypes)
        flag(OL$Warning, "Nonlinear part of the specified transformation is not invertible")
    
    if ("affine" %in% availableTypes)
        affineMatrices <- lapply(transform$getAffineMatrices(), invertAffine)
    
    newTransform <- Transformation$new(transform$getTargetImage(), transform$getSourceImage(), affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method=transform$getMethod())
    return (newTransform)
}

decomposeTransformation <- function (transform)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    if (!("affine" %in% transform$getTypes()))
        report(OL$Error, "Decomposition can only be performed for affine transformations")
    
    return (lapply(transform$getAffineMatrices(), decomposeAffine))
}

mergeTransformations <- function (transforms, newSourceImage)
{
    if (!is.list(transforms))
        report(OL$Error, "Transformations must be specified in a list")
    if (length(transforms) == 0)
        report(OL$Error, "At least one transformation must be given")
    
    methods <- sapply(transforms, function(x) x$getMethod())
    if (!all(methods == methods[1]))
        report(OL$Error, "Method must be the same for all transformations")
    
    pad <- function (x, len)
    {
        if (length(x) == len)
            x
        else
            c(x, rep(list(NULL),len-length(x)))
    }
    
    affineMatrices <- do.call("c", lapply(transforms, function(x) pad(x$getAffineMatrices(),x$nRegistrations())))
    controlPointImages <- do.call("c", lapply(transforms, function(x) pad(x$getControlPointImages(),x$nRegistrations())))
    reverseControlPointImages <- do.call("c", lapply(transforms, function(x) pad(x$getReverseControlPointImages(),x$nRegistrations())))
    
    transform <- Transformation$new(newSourceImage, transforms[[1]]$getTargetImage(), affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method=methods[1])
    return (transform)
}
