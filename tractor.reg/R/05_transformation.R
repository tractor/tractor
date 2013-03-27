Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
    getAffineMatrix = function (i = NULL)
    {
        if (is.null(i))
        {
            mat <- affineMatrices
            mat <- lapply(mat, function(m) {
                attr(m, "affineType") <- ifelse(method=="flirt", "fsl", "niftyreg")
                return (m)
            })
            return (mat)
        }
        else
        {
            mat <- affineMatrices[[i]]
            attr(mat, "affineType") <- ifelse(method=="flirt", "fsl", "niftyreg")
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

registerImages <- function (sourceImage, targetImage, targetMask = NULL, method = c("niftyreg","flirt"), types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, finalInterpolation = 1, cache = c("auto","read","write","ignore"), ...)
{
    method <- match.arg(method)
    types <- match.arg(types, several.ok=TRUE)
    cache <- match.arg(cache)
    
    transform <- NULL
    cacheHit <- FALSE
    
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
            result <- applyTransformation(transform, finalInterpolation=finalInterpolation, ...)
    }
    else if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(getImageAsObject(sourceImage), getImageAsObject(targetImage), targetMask=getImageAsObject(targetMask), types=types, affineDof=affineDof, estimateOnly=estimateOnly, finalInterpolation=finalInterpolation, ...)
    else if (method == "flirt")
    {
        if (any(c("nonlinear","reverse-nonlinear") %in% types))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        
        sourceFileName <- getImageAsFileName(sourceImage, warnIfNotLas=TRUE)
        targetFileName <- getImageAsFileName(targetImage, warnIfNotLas=TRUE)
        targetMaskFileName <- getImageAsFileName(targetMask, warnIfNotLas=TRUE)
        
        result <- registerImagesWithFlirt(sourceFileName, targetFileName, targetMaskFileName=targetMaskFileName, affineDof=affineDof, estimateOnly=estimateOnly, finalInterpolation=finalInterpolation, ...)
    }
    
    if (cache == "write" || (cache == "auto" && !cacheHit))
        updateTransformationCache(result$transform, force=TRUE)
    
    return (result)
}

resampleImageToDimensions <- function (image, voxelDims = NULL, imageDims = NULL, origin = NULL, finalInterpolation = 1)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        report(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    if (is.null(imageDims))
        imageDims <- round(image$getFieldOfView() / abs(voxelDims))
    
    targetImage <- MriImage$new(imageDims=imageDims, voxelDims=voxelDims, voxelDimUnits=image$getVoxelUnits(), origin=origin)
    
    options <- list(nLevels=0, verbose=FALSE, scope="affine")
    result <- registerImagesWithNiftyreg(image, targetImage, initAffine=NULL, types="affine", estimateOnly=FALSE, finalInterpolation=finalInterpolation, linearOptions=options)
    
    return (result$transformedImage)
}

applyTransformation <- function (transform, newImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, finalInterpolation = 1)
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

invertTransformation <- function (transform)
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
    else if ("nonlinear" %in% availableTypes)
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
        return (list(decomposeAffine(transform$getAffineMatrix(1), as(transform$getSourceImage(),"nifti"), as(transform$getTargetImage(),"nifti"))))
    else
    {
        targetImageNifti <- as(transform$getTargetImage(), "nifti")
        result <- lapply(seq_len(transform$getSourceImage()$getDimensions()[nSourceDims]), function (i) {
            currentSourceImage <- as.nifti(extractDataFromMriImage(transform$getSourceImage(),nSourceDims,i), as(transform$getSourceImage(),"nifti"))
            decomposeAffine(transform$getAffineMatrix(i), currentSourceImage, targetImageNifti)
        })
        return (result)
    }
}
