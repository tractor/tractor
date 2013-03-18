setClassUnion("MriImageOrNull", c("MriImage","NULL"))

Registration <- setRefClass("Registration", contains="SerialisableObject", fields=list(sourceImage="MriImage",targetImage="MriImage",transformedImage="MriImage",reverseTransformedImage="MriImageOrNull",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list",method="character"), methods=list(
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
    
    getReverseTransformedImage = function () { return (reverseTransformedImage) },
    
    getSourceImage = function () { return (sourceImage) },
    
    getStoredTransformations = function ()
    {
        transformTypeNames <- c("affine", "nonlinear", "reverse-nonlinear")
        availability <- sapply(list(affineMatrices,controlPointImages,reverseControlPointImages), length) > 0
        return (transformTypeNames[availability])
    },
    
    getTargetImage = function () { return (targetImage) },
    
    getTransformedImage = function () { return (transformedImage) },
    
    summarise = function ()
    {
        sourceSummary <- sourceImage$summarise()
        targetSummary <- targetImage$summarise()
        
        values <- c(sourceSummary$values[match(c("Image dimensions","Voxel dimensions"), sourceSummary$labels)], targetSummary$values[match(c("Image dimensions","Voxel dimensions"), targetSummary$labels)])
        values <- c(values, .self$getMethod(), implode(.self$getStoredTransformations,", "))
        names(values) <- c("Source image dimensions", "Source voxel dimensions", "Target image dimensions", "Target voxel dimensions", "Registration method", "Stored transformations")
        
        return (values)
    }
))

newRegistrationWithMethod <- function (method = c("niftyreg","flirt"), sourceImage, targetImage, targetMask = NULL, transformTypes = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, useCache = TRUE, forceCacheUpdate = FALSE, ...)
{
    method <- match.arg(method)
    transformTypes <- match.arg(transformTypes, several.ok=TRUE)
    
    if (useCache)
    {
        registration <- checkRegistrationCache(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), method, transformTypes)
        if (!is.null(registration))
            return (registration)
    }
    
    if (method == "niftyreg")
        registration <- newRegistrationWithNiftyreg(getImageAsObject(sourceImage), getImageAsObject(targetImage), targetMask=getImageAsObject(targetMask), transformTypes=transformTypes, affineDof=affineDof, ...)
    else if (method == "flirt")
        registration <- newRegistrationWithFlirt(getImageAsFileName(sourceImage), getImageAsFileName(targetImage), targetMask=getImageAsFileName(targetMask), transformTypes=transformTypes, affineDof=affineDof, ...)
    
    if (useCache)
        updateRegistrationCache(registration, force=forceCacheUpdate)
    
    return (registration)
}
