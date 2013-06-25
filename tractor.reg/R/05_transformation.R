Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
    getAffineMatrix = function (i = NULL)
    {
        if (is.null(i))
        {
            mat <- affineMatrices
            mat <- lapply(mat, function(m) {
                attr(m, "affineType") <- method
                return (m)
            })
            return (mat)
        }
        else
        {
            mat <- affineMatrices[[i]]
            attr(mat, "affineType") <- method
            return (mat)
        }
    },
    
    getControlPointImage = function (i = NULL)
    {
        if (is.null(i))
            return (controlPointImages)
        else
            return (controlPointImages[[i]])
    },
    
    getMethod = function () { return (method) },
    
    getReverseControlPointImage = function (i = NULL)
    {
        if (is.null(i))
            return (reverseControlPointImages)
        else
            return (reverseControlPointImages[[i]])
    },
    
    getSourceImage = function () { return (sourceImage) },
    
    getTargetImage = function () { return (targetImage) },
    
    getTypes = function ()
    {
        transformTypeNames <- c("affine", "nonlinear", "reverse-nonlinear")
        availability <- sapply(list(affineMatrices,controlPointImages,reverseControlPointImages), length) > 0
        return (transformTypeNames[availability])
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
    
    availableTypes <- x$getTypes()
    affine <- controlPointImage <- NULL
    
    if (is.null(sourceImage))
    {
        if (reverse)
            sourceImage <- x$getTargetImage()
        else
            sourceImage <- x$getSourceImage()
        
        if (sourceImage$isEmpty())
        {
            if (sourceImage$isInternal())
                report(OL$Error, "Original image source is unknown - a source image must be provided")
            sourceImage <- readImageFile(sourceImage$getSource())
        }
        else if (!sourceImage$isReordered())
            sourceImage <- newMriImageByReordering(sourceImage)
    }
    
    if (preferAffine && ("affine" %in% availableTypes))
        affine <- x$getAffineMatrix(index)
    else if (reverse && ("reverse-nonlinear" %in% availableTypes))
        controlPointImage <- x$getReverseControlPointImage(index)
    else if (!reverse && ("nonlinear" %in% availableTypes))
        controlPointImage <- x$getControlPointImage(index)
    else if ("affine" %in% availableTypes)
        affine <- x$getAffineMatrix(index)
    else
        report(OL$Error, "The specified Transformation object does not contain the necessary information")
    
    if (!is.null(affine) && reverse)
        affine <- invertAffine(affine)
    
    # Calculate the deformation field
    if (reverse)
        deformationField <- getDeformationField(x$getSourceImage(), affine=affine, controlPointImage=controlPointImage, jacobian=TRUE)
    else
        deformationField <- getDeformationField(x$getTargetImage(), affine=affine, controlPointImage=controlPointImage, jacobian=TRUE)
    
    # Find the requested slice of the Jacobian map and deformation field
    jacobian <- extractDataFromMriImage(newMriImageByReordering(as(deformationField$jacobian,"MriImage")), throughPlaneAxis, loc[throughPlaneAxis])
    field <- extractDataFromMriImage(newMriImageByReordering(as(deformationField$deformationField,"MriImage")), throughPlaneAxis, loc[throughPlaneAxis])
    
    # Remove the last row and column from the data, because NiftyReg seems to calculate it wrongly
    fieldDims <- dim(field)
    jacobian <- jacobian[1:(fieldDims[1]-1),1:(fieldDims[2]-1)]
    field <- field[1:(fieldDims[1]-1),1:(fieldDims[2]-1),,]
    
    # Convert the field to voxel positions and find the closest plane (on average) in source space
    fieldDims <- dim(field)
    dim(field) <- c(prod(fieldDims[1:2]), fieldDims[3])
    fieldVoxels <- reorderPoints(transformWorldToVoxel(field,sourceImage$getMetadata()), sourceImage$getMetadata())
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

registerImages <- function (sourceImage, targetImage, targetMask = NULL, method = getOption("tractorRegistrationMethod"), types = "affine", affineDof = 12, estimateOnly = FALSE, finalInterpolation = 1, cache = c("auto","read","write","ignore"), file = NULL, ...)
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
            result <- list(transform=transform, transformedImage=transformImage(transform,finalInterpolation=finalInterpolation,...))
    }
    else if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(getImageAsObject(sourceImage,reorder=FALSE), getImageAsObject(targetImage,reorder=FALSE), targetMask=getImageAsObject(targetMask,allowNull=TRUE,reorder=FALSE), types=types, affineDof=affineDof, estimateOnly=estimateOnly, finalInterpolation=finalInterpolation, ...)
    else if (method == "fsl")
    {
        if (any(c("nonlinear","reverse-nonlinear") %in% types))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        
        result <- registerImagesWithFlirt(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), targetMaskFileName=getImageAsFileName(targetMask,allowNull=TRUE), affineDof=affineDof, estimateOnly=estimateOnly, finalInterpolation=finalInterpolation, ...)
    }
    
    if (cache == "write" || (cache == "auto" && !cacheHit))
        updateTransformationCache(result$transform, force=TRUE)
    
    if (!is.null(file) && !fileHit)
        result$transform$serialise(file)
    
    return (result)
}

resampleImage <- function (image, voxelDims = NULL, imageDims = NULL, origin = NULL, finalInterpolation = 1)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        report(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    if (is.null(imageDims))
        imageDims <- round(image$getFieldOfView() / abs(voxelDims))
    
    targetImage <- getRefClass("MriImage")$new(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=image$getVoxelUnits(), origin=origin)
    
    options <- list(nLevels=0, verbose=FALSE, scope="affine")
    result <- registerImagesWithNiftyreg(image, targetImage, initAffine=NULL, types="affine", estimateOnly=FALSE, finalInterpolation=finalInterpolation, linearOptions=options)
    
    return (result$transformedImage)
}

invertTransformation <- function (transform, quiet = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    affineMatrices <- controlPointImages <- reverseControlPointImages <- list()
    
    availableTypes <- transform$getTypes()
    if (all(c("nonlinear","reverse-nonlinear") %in% availableTypes))
    {
        controlPointImages <- transform$getReverseControlPointImage()
        reverseControlPointImages <- transform$getControlPointImage()
    }
    else if (!quiet && "nonlinear" %in% availableTypes)
        flag(OL$Warning, "Nonlinear part of the specified transformation is not invertible")
    
    if ("affine" %in% availableTypes)
        affineMatrices <- lapply(transform$getAffineMatrix(), invertAffine)
    
    newTransform <- Transformation$new(sourceImage=transform$getTargetImage(), targetImage=transform$getSourceImage(), affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method=transform$getMethod())
    return (newTransform)
}

decomposeTransformation <- function (transform)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    if (!("affine" %in% transform$getTypes()))
        report(OL$Error, "Decomposition can only be performed for affine transformations")
    
    nSourceDims <- transform$getSourceImage()$getDimensionality()
    nTargetDims <- transform$getTargetImage()$getDimensionality()
    if (nSourceDims == nTargetDims)
        return (list(decomposeAffine(transform$getAffineMatrix(1), transform$getSourceImage(), transform$getTargetImage())))
    else
    {
        targetImageNifti <- as(transform$getTargetImage(), "nifti")
        result <- lapply(seq_len(transform$getSourceImage()$getDimensions()[nSourceDims]), function (i) {
            currentSourceImage <- newMriImageByExtraction(transform$getSourceImage(), nSourceDims, i)
            decomposeAffine(transform$getAffineMatrix(i), currentSourceImage, targetImageNifti)
        })
        return (result)
    }
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
    
    affineMatrices <- do.call("c", lapply(transforms, function(x) x$getAffineMatrix()))
    controlPointImages <- do.call("c", lapply(transforms, function(x) x$getControlPointImage()))
    reverseControlPointImages <- do.call("c", lapply(transforms, function(x) x$getReverseControlPointImage()))
    
    transform <- Transformation$new(sourceImage=newSourceImage, targetImage=transforms[[1]]$getTargetImage(), affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method=methods[1])
    return (transform)
}
