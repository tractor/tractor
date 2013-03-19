newTransformationWithNiftyreg <- function (sourceImage, targetImage, targetMask = NULL, initAffine = NULL, transformTypes = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, linearOptions = list(), nonlinearOptions = list())
{
    if (!is(sourceImage,"MriImage") || !is(targetImage,"MriImage"))
        report(OL$Error, "Source and target images must be specified as MriImage objects")
    if (sourceImage$isEmpty() || targetImage$isEmpty())
        report(OL$Error, "Source and target images must not be empty")
    
    transformTypes <- match.arg(transformTypes, several.ok=TRUE)
    
    # Run the linear part of the registration, unless nonlinear-only is explicitly requested
    if ("affine" %in% transformTypes)
    {
        if (!any(affineDof == c(6,12)))
            report(OL$Error, "Only 6 and 12 degrees of freedom are supported by NiftyReg")
        
        linearOptions$source <- as(sourceImage, "nifti")
        linearOptions$target <- as(targetImage, "nifti")
        if (!is.null(targetMask))
            linearOptions$targetMask <- as(targetMask, "nifti")
        linearOptions$initAffine <- initAffine
        linearOptions$scope <- ifelse(affineDof==6, "rigid", "affine")
        if (is.null(linearOptions$estimateOnly))
            linearOptions$estimateOnly <- estimateOnly
        
        startTime <- Sys.time()
        result <- do.call("niftyreg.linear", linearOptions)
        endTime <- Sys.time()
        report(OL$Verbose, "Linear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
        
        # Update affine initialisation from result
        initAffine <- result$affine
    }
    
    # Run the nonlinear part of the registration, if required
    if ("nonlinear" %in% transformTypes)
    {
        nonlinearOptions$source <- as(sourceImage, "nifti")
        nonlinearOptions$target <- as(targetImage, "nifti")
        if (!is.null(targetMask))
            nonlinearOptions$targetMask <- as(targetMask, "nifti")
        nonlinearOptions$initAffine <- initAffine
        if (is.null(nonlinearOptions$estimateOnly))
            nonlinearOptions$estimateOnly <- estimateOnly
        
        startTime <- Sys.time()
        result <- do.call("niftyreg.nonlinear", nonlinearOptions)
        endTime <- Sys.time()
        report(OL$Verbose, "Nonlinear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    }
    
    if (!is.null(result$control))
        result$control <- lapply(result$control, function(x) as(x,"MriImage"))
    if (!is.null(result$reverseImage))
        result$reverseImage <- as(result$reverseImage, "MriImage")
    if (!is.null(result$reverseControl))
        result$reverseControl <- lapply(result$reverseControl, function(x) as(x,"MriImage"))
    
    transform <- Transformation$new(sourceImage=sourceImage, targetImage=targetImage, affineMatrices=as.list(result$affine), controlPointImages=as.list(result$control), reverseControlPointImages=as.list(result$reverseControl), method="niftyreg")
    return (list(transform=transform, transformedImage=as(result$image,"MriImage"), reverseTransformedImage=result$reverseImage))
}
