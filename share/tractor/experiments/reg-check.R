#@desc Visualise the result of a registration by overlaying the "morphological gradient" of the transformed source image, which shows up edges, on the target image.
#@args transform directory, [source image file]

library(tractor.reg)
library(mmand)

runExperiment <- function ()
{
    requireArguments("transform directory")
    
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    reverse <- getConfigVariable("Reverse", FALSE)
    
    transform <- attachTransformation(Arguments[1])
    
    if (nArguments() > 1)
        sourceImage <- readImageFile(implode(Arguments[-1]," "))
    else
        sourceImage <- transform$getSourceImage(reverse=reverse, reorder=TRUE)
    
    targetImage <- transform$getTargetImage(reverse=reverse, reorder=TRUE)
    
    transformedImage <- transformImage(transform, sourceImage, preferAffine=preferAffine, reverse=reverse, interpolation=1)
    kernel <- shapeKernel(3, min(3,transformedImage$getDimensionality()), type="diamond")
    gradient <- dilate(transformedImage,kernel) - erode(transformedImage,kernel)
    gradient[!is.finite(gradient)] <- 0
    outlineImage <- asMriImage(threshold(gradient,method="kmeans",binarise=FALSE), transformedImage)
    
    writeImageFile(outlineImage, paste(basename(sourceImage$getSource()),"outline",sep="_"))
    
    loc <- round(dim(targetImage) / 2)
    prefix <- paste(basename(sourceImage$getSource()), "overlay", sep="_")
    tractor.base:::compositeImages(list(targetImage,outlineImage), x=loc[1], y=loc[2], z=loc[3], colourScales="red", projectOverlays=FALSE, alpha=0.5, prefix=prefix)
}
