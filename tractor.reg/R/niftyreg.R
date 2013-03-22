registerImagesWithNiftyreg <- function (sourceImage, targetImage, targetMask = NULL, initAffine = NULL, types = c("affine","nonlinear","reverse-nonlinear"), affineDof = 12, estimateOnly = FALSE, linearOptions = list(), nonlinearOptions = list())
{
    if (!is(sourceImage,"MriImage") || !is(targetImage,"MriImage"))
        report(OL$Error, "Source and target images must be specified as MriImage objects")
    
    types <- match.arg(types, several.ok=TRUE)
    
    linearResult <- nonlinearResult <- list()
    
    # Run the linear part of the registration, unless nonlinear-only is explicitly requested
    if ("affine" %in% types)
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
        linearResult <- do.call("niftyreg.linear", linearOptions)
        endTime <- Sys.time()
        report(OL$Verbose, "Linear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
        
        # Update affine initialisation from result
        initAffine <- linearResult$affine
    }
    
    # Run the nonlinear part of the registration, if required
    if ("nonlinear" %in% types)
    {
        nonlinearOptions$source <- as(sourceImage, "nifti")
        nonlinearOptions$target <- as(targetImage, "nifti")
        if (!is.null(targetMask))
            nonlinearOptions$targetMask <- as(targetMask, "nifti")
        nonlinearOptions$initAffine <- initAffine
        if (is.null(nonlinearOptions$estimateOnly))
            nonlinearOptions$estimateOnly <- estimateOnly
        
        startTime <- Sys.time()
        nonlinearResult <- do.call("niftyreg.nonlinear", nonlinearOptions)
        endTime <- Sys.time()
        report(OL$Verbose, "Nonlinear registration completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    }
    
    if (!is.null(nonlinearResult$control))
        nonlinearResult$control <- lapply(nonlinearResult$control, function(x) as(x,"MriImage"))
    if (!is.null(nonlinearResult$reverseImage))
        nonlinearResult$reverseImage <- as(nonlinearResult$reverseImage, "MriImage")
    if (!is.null(nonlinearResult$reverseControl))
        nonlinearResult$reverseControl <- lapply(nonlinearResult$reverseControl, function(x) as(x,"MriImage"))
    
    if (estimateOnly)
        transformedImage <- NULL
    else if (!is.null(nonlinearResult$image))
        transformedImage <- as(nonlinearResult$image, "MriImage")
    else
        transformedImage <- as(linearResult$image, "MriImage")
    
    transform <- Transformation$new(sourceImage=sourceImage, targetImage=targetImage, affineMatrices=as.list(linearResult$affine), controlPointImages=as.list(nonlinearResult$control), reverseControlPointImages=as.list(nonlinearResult$reverseControl), method="niftyreg")
    return (list(transform=transform, transformedImage=transformedImage, reverseTransformedImage=nonlinearResult$reverseImage))
}
