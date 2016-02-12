.interpolationNameToCode <- function (interpolation)
{
    if (is.character(interpolation))
        code <- switch(interpolation, nearestneighbour=0, trilinear=1, sinc=2, spline=3, cubicspline=3, NULL)
    else
        code <- interpolation
    
    if (code == 2)
        report(OL$Error, "NiftyReg does not support sinc interpolation")
    else if (is.null(code))
        report(OL$Error, "Interpolation type \"#{interpolation}\" is not valid")
    
    return (code)
}

registerImagesWithNiftyreg <- function (sourceImage, targetImage, sourceMask = NULL, targetMask = NULL, init = NULL, types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, interpolation = 1, linearOptions = list(), nonlinearOptions = list())
{
    .formatImage <- function(x) { if (is(x,"MriImage")) as(x,"niftiImage") else x }
    
    types <- match.arg(types, several.ok=TRUE)
    interpolation <- .interpolationNameToCode(interpolation)
    
    linearResult <- nonlinearResult <- list()
    
    # Run the linear part of the registration, unless nonlinear-only is explicitly requested
    if ("affine" %in% types)
    {
        if (!any(affineDof == c(6,12)))
            report(OL$Error, "Only 6 and 12 degrees of freedom are supported by NiftyReg")
        
        linearOptions$source <- .formatImage(sourceImage)
        linearOptions$target <- .formatImage(targetImage)
        linearOptions$sourceMask <- .formatImage(sourceMask)
        linearOptions$targetMask <- .formatImage(targetMask)
        linearOptions$init <- init
        linearOptions$scope <- ifelse(affineDof==6, "rigid", "affine")
        if (is.null(linearOptions$estimateOnly))
            linearOptions$estimateOnly <- estimateOnly
        if (is.null(linearOptions$interpolation) && !is.null(interpolation))
            linearOptions$interpolation <- interpolation
        
        startTime <- Sys.time()
        linearResult <- do.call("niftyreg.linear", linearOptions)
        endTime <- Sys.time()
        report(OL$Info, "Linear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
        
        # Update affine initialisation from the result
        init <- forward(linearResult)
    }
    
    # Run the nonlinear part of the registration, if required
    if ("nonlinear" %in% types)
    {
        nonlinearOptions$source <- .formatImage(sourceImage)
        nonlinearOptions$target <- .formatImage(targetImage)
        nonlinearOptions$sourceMask <- .formatImage(sourceMask)
        nonlinearOptions$targetMask <- .formatImage(targetMask)
        nonlinearOptions$init <- .formatImage(init)
        if (is.null(nonlinearOptions$estimateOnly))
            nonlinearOptions$estimateOnly <- estimateOnly
        if (is.null(nonlinearOptions$interpolation) && !is.null(interpolation))
            nonlinearOptions$interpolation <- interpolation
        nonlinearOptions$symmetric <- ("reverse-nonlinear" %in% types)
        
        startTime <- Sys.time()
        nonlinearResult <- do.call("niftyreg.nonlinear", nonlinearOptions)
        endTime <- Sys.time()
        report(OL$Info, "Nonlinear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    }
    
    affineMatrices <- controlPointImages <- reverseControlPointImages <- NULL
    
    if (!is.null(linearResult$forwardTransforms))
        affineMatrices <- linearResult$forwardTransforms
    else if (isAffine(init))
        affineMatrices <- list(init)
    else if (is.list(init))
        affineMatrices <- lapply(init, function(x) { if (isAffine(x)) x else NULL })
    
    if (!is.null(nonlinearResult$forwardTransforms))
        controlPointImages <- lapply(nonlinearResult$forwardTransforms, function(x) as(x,"MriImage"))
    if (!is.null(nonlinearResult$reverseTransforms))
        reverseControlPointImages <- lapply(nonlinearResult$reverseTransforms, function(x) as(x,"MriImage"))
    
    transformedImage <- reverseTransformedImage <- NULL
    if (!estimateOnly)
    {
        if (!is.null(nonlinearResult$image))
            transformedImage <- as(nonlinearResult$image, "MriImage")
        else
            transformedImage <- as(linearResult$image, "MriImage")
        if (!is.null(nonlinearResult$reverseTransforms) && ndim(sourceImage) == ndim(targetImage))
            reverseTransformedImage <- as(applyTransform(reverse(nonlinearResult), nonlinearResult$target, interpolation=interpolation), "MriImage")
    }
    
    transform <- Transformation$new(sourceImage$getMetadata(), targetImage$getMetadata(), affineMatrices=affineMatrices, controlPointImages=controlPointImages, reverseControlPointImages=reverseControlPointImages, method="niftyreg")
    return (list(transform=transform, transformedImage=transformedImage, reverseTransformedImage=reverseTransformedImage))
}
