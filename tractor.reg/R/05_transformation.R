Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
    getAffineMatrix = function (i = NULL)
    {
        if (is.null(i))
        {
            mat <- affineMatrices
            mat <- lapply(mat, function(m) {
                attr(m, "affineType") <- ifelse(method=="flirt", "fsl", "niftyreg"))
                return (m)
            }
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
        values <- c(values, .self$getMethod(), implode(.self$getTypes(),", "))
        names(values) <- c("Source image dimensions", "Source voxel dimensions", "Target image dimensions", "Target voxel dimensions", "Registration method", "Stored transformations")
        
        return (values)
    }
))

registerImages <- function (sourceImage, targetImage, targetMask = NULL, method = c("niftyreg","flirt"), types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, cache = c("auto","read","write","ignore"), ...)
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
            result <- applyTransformation(transform, ...)
    }
    else if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(getImageAsObject(sourceImage), getImageAsObject(targetImage), targetMask=getImageAsObject(targetMask), types=types, affineDof=affineDof, estimateOnly=estimateOnly, ...)
    else if (method == "flirt")
    {
        if (any(c("nonlinear","reverse-nonlinear") %in% types))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        
        sourceFileName <- getImageAsFileName(sourceImage, warnIfNotLas=TRUE)
        targetFileName <- getImageAsFileName(targetImage, warnIfNotLas=TRUE)
        targetMaskFileName <- getImageAsFileName(targetMask, warnIfNotLas=TRUE)
        
        result <- registerImagesWithFlirt(sourceFileName, targetFileName, targetMaskFileName=targetMaskFileName, affineDof=affineDof, estimateOnly=estimateOnly, ...)
    }
    
    if (cache == "write" || (cache == "auto" && !cacheHit))
        updateTransformationCache(result$transform, force=TRUE)
    
    return (result)
}

applyTransformation <- function (transform, newImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, interpolation = 3)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    options <- list(nLevels=0, finalInterpolation=interpolation, verbose=FALSE)
    
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
        
        result <- registerImagesWithNiftyreg(sourceImage, targetImage, initAffine=initAffine, types="affine", estimateOnly=FALSE, linearOptions=options)
    }
    else
        result <- registerImagesWithNiftyreg(sourceImage, targetImage, types="nonlinear", estimateOnly=FALSE, nonlinearOptions=options)
    
    return (result)
}
