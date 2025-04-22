.interpolationNameToCode <- function (interpolation)
{
    if (is.character(interpolation))
        code <- switch(interpolation, nearestneighbour=0L, trilinear=1L, sinc=2L, spline=3L, cubicspline=3L, NULL)
    else
        code <- interpolation
    
    assert(!is.null(code), "Interpolation type \"#{interpolation}\" is not valid")
    assert(code != 2, "NiftyReg does not support sinc interpolation")
    return (code)
}

registerImagesWithNiftyreg <- function (registration, sourceMask = NULL, targetMask = NULL, init = NULL, types = TransformTypes, affineDof = 12, estimateOnly = FALSE, interpolation = 1, linearOptions = list(), nonlinearOptions = list())
{
    types <- match.arg(types, several.ok=TRUE)
    interpolation <- .interpolationNameToCode(interpolation)
    
    linearResult <- nonlinearResult <- list()
    
    # Run the linear part of the registration, unless nonlinear-only is explicitly requested
    if ("affine" %in% types)
    {
        if (!any(affineDof == c(6,12)))
            report(OL$Error, "Only 6 and 12 degrees of freedom are supported by NiftyReg")
        
        linearOptions$source <- registration$getSource()
        linearOptions$target <- registration$getTarget()
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
        
        xfms <- lapply(seq_along(linearResult$forwardTransforms), function(i) forward(linearResult,i))
        registration$setTransforms(xfms, "affine")
        
        # Update affine initialisation from the result
        init <- xfms
    }
    
    # Run the nonlinear part of the registration, if required
    if ("nonlinear" %in% types)
    {
        if (is.null(init))
            flag(OL$Warning, "Running nonlinear registration without initialisation is not recommended")
        
        nonlinearOptions$source <- registration$getSourceImage()
        nonlinearOptions$target <- registration$getTargetImage()
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
        
        xfms <- lapply(seq_along(nonlinearResult$forwardTransforms), function(i) forward(nonlinearResult,i))
        registration$setTransforms(xfms, "nonlinear")
        if (nonlinearOptions$symmetric)
        {
            xfms <- lapply(seq_along(nonlinearResult$reverseTransforms), function(i) reverse(nonlinearResult,i))
            registration$setTransforms(xfms, "reverse-nonlinear")
        }
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
    
    return (list(registration=registration, transformedImage=transformedImage, reverseTransformedImage=reverseTransformedImage))
}
