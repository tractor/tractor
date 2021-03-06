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

registerImagesWithNiftyreg <- function (transform, sourceMask = NULL, targetMask = NULL, init = NULL, types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, interpolation = 1, linearOptions = list(), nonlinearOptions = list())
{
    types <- match.arg(types, several.ok=TRUE)
    interpolation <- .interpolationNameToCode(interpolation)
    
    linearResult <- nonlinearResult <- list()
    
    # Run the linear part of the registration, unless nonlinear-only is explicitly requested
    if ("affine" %in% types)
    {
        if (!any(affineDof == c(6,12)))
            report(OL$Error, "Only 6 and 12 degrees of freedom are supported by NiftyReg")
        
        linearOptions$source <- transform$getSourceImage()
        linearOptions$target <- transform$getTargetImage()
        linearOptions$sourceMask <- sourceMask
        linearOptions$targetMask <- targetMask
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
        
        transform$updateFromResult(linearResult)
        
        # Update affine initialisation from the result
        init <- forward(linearResult)
    }
    
    # Run the nonlinear part of the registration, if required
    if ("nonlinear" %in% types)
    {
        if (is.null(init))
            flag(OL$Warning, "Running nonlinear registration without initialisation is not recommended")
        
        nonlinearOptions$source <- transform$getSourceImage()
        nonlinearOptions$target <- transform$getTargetImage()
        nonlinearOptions$sourceMask <- sourceMask
        nonlinearOptions$targetMask <- targetMask
        nonlinearOptions$init <- init
        if (is.null(nonlinearOptions$estimateOnly))
            nonlinearOptions$estimateOnly <- estimateOnly
        if (is.null(nonlinearOptions$interpolation) && !is.null(interpolation))
            nonlinearOptions$interpolation <- interpolation
        nonlinearOptions$symmetric <- ("reverse-nonlinear" %in% types)
        
        startTime <- Sys.time()
        nonlinearResult <- do.call("niftyreg.nonlinear", nonlinearOptions)
        endTime <- Sys.time()
        report(OL$Info, "Nonlinear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
        
        transform$updateFromResult(nonlinearResult)
    }
    
    transformedImage <- reverseTransformedImage <- NULL
    if (!estimateOnly)
    {
        if (!is.null(nonlinearResult$image))
            transformedImage <- as(nonlinearResult$image, "MriImage")
        else
            transformedImage <- as(linearResult$image, "MriImage")
        if (!is.null(nonlinearResult$reverseTransforms) && length(nonlinearResult$reverseTransforms) == 1)
            reverseTransformedImage <- as(applyTransform(reverse(nonlinearResult), nonlinearResult$target, interpolation=interpolation), "MriImage")
    }
    
    return (list(transform=transform, transformedImage=transformedImage, reverseTransformedImage=reverseTransformedImage))
}
