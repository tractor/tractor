#@desc Visualise the result of a registration by overlaying the "morphological gradient" of the transformed source image, which shows up edges, on the target image.
#@args registration file, [source image file]
#@group Registration

library(tractor.reg)
library(mmand)

runExperiment <- function ()
{
    requireArguments("transform directory")
    
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    reverse <- getConfigVariable("Reverse", FALSE)
    
    registration <- readRegistration(Arguments[1])
    transform <- registration$getTransforms(preferAffine=preferAffine, reverse=reverse)
    
    if (nArguments() > 1)
        sourceImage <- readImageFile(implode(Arguments[-1]," "))
    else
        sourceImage <- reorderMriImage(registration$getSource(reverse))
    
    targetImage <- reorderMriImage(registration$getTarget(reverse))
    
    transformedImage <- transformImage(transform, sourceImage, interpolation=1)
    kernel <- shapeKernel(3, min(3,transformedImage$getDimensionality()), type="diamond")
    gradient <- dilate(transformedImage,kernel) - erode(transformedImage,kernel)
    gradient[!is.finite(gradient)] <- 0
    outlineImage <- asMriImage(threshold(gradient,method="kmeans",binarise=FALSE), transformedImage)
    
    fileStem <- where(sourceImage$isInternal(), "", paste0(basename(sourceImage$getSource()), "_"))
    writeImageFile(outlineImage, paste0(fileStem,"outline"))
    
    loc <- round(dim(targetImage) / 2)
    tractor.base:::compositeImages(list(targetImage,outlineImage), x=loc[1], y=loc[2], z=loc[3], colourScales="red", projectOverlays=FALSE, alpha=0.5, prefix=paste0(fileStem,"overlay"))
}
