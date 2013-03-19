Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
    getAffineMatrix = function (i = NULL)
    {
        if (is.null(i))
            return (affineMatrices)
        else
            return (affineMatrices[[i]])
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

newTransformationWithMethod <- function (method = c("niftyreg","flirt"), sourceImage, targetImage, targetMask = NULL, types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, useCache = TRUE, forceCacheUpdate = FALSE, ...)
{
    method <- match.arg(method)
    types <- match.arg(types, several.ok=TRUE)
    
    transform <- NULL
    
    if (useCache)
    {
        transform <- checkTransformationCache(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), method, types)
        if (estimateOnly && !is.null(transform))
            return (list(transform=transform, transformedImage=NULL, reverseTransformedImage=NULL))
    }
    
    if (method == "niftyreg")
        result <- newTransformationWithNiftyreg(getImageAsObject(sourceImage), getImageAsObject(targetImage), targetMask=getImageAsObject(targetMask), types=types, affineDof=affineDof, estimateOnly=estimateOnly, ...)
    else if (method == "flirt")
        result <- newTransformationWithFlirt(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), targetMask=getImageAsFileName(targetMask), types=types, affineDof=affineDof, estimateOnly=estimateOnly, ...)
    
    if (useCache)
        updateTransformationCache(result$registration, force=forceCacheUpdate)
    
    return (result)
}
