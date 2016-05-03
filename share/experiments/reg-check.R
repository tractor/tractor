#@desc Visualise the result of a registration by overlaying the "morphological gradient" of the transformed source image, which shows up edges, on the target image.
#@args [source image file, target image file]

library(tractor.reg)
library(mmand)

runExperiment <- function ()
{
    transformName <- getConfigVariable("TransformationName", NULL, "character")
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    reverse <- getConfigVariable("Reverse", FALSE)
    
    transform <- attachTransformation(transformName)
    
    if (nArguments() > 0)
        sourceImage <- readImageFile(Arguments[1])
    else
    {
        sourceImage <- transform$getSourceImage(reverse=reverse)
        if (sourceImage$isEmpty())
        {
            if (sourceImage$isInternal())
                report(OL$Error, "Original image source is unknown - a source image must be provided")
            sourceImage <- readImageFile(sourceImage$getSource())
        }
        else if (!sourceImage$isReordered())
            sourceImage <- reorderMriImage(sourceImage)
    }
    
    if (nArguments() > 1)
        targetImage <- readImageFile(Arguments[2])
    else
    {
        targetImage <- transform$getTargetImage(reverse=reverse)
        if (targetImage$isEmpty())
        {
            if (targetImage$isInternal())
                report(OL$Error, "Original image source is unknown - a source image must be provided")
            targetImage <- readImageFile(targetImage$getSource())
        }
        else if (!targetImage$isReordered())
            targetImage <- reorderMriImage(targetImage)
    }
    
    transformedImage <- transformImage(transform, sourceImage, preferAffine=preferAffine, reverse=reverse, interpolation=1)
    kernel <- shapeKernel(3, min(3,transformedImage$getDimensionality()), type="diamond")
    gradient <- dilate(transformedImage,kernel) - erode(transformedImage,kernel)
    gradient[!is.finite(gradient)] <- 0
    outlineImage <- asMriImage(threshold(gradient,method="kmeans",binarise=FALSE), transformedImage)
    
    writeImageFile(outlineImage, paste(basename(sourceImage$getSource()),"outline",sep="_"))
    
    loc <- round(dim(targetImage) / 2)
    prefix <- paste(basename(sourceImage$getSource()), "overlay", sep="_")
    tractor.base:::compositeImages(list(targetImage,outlineImage), x=loc[1], y=loc[2], z=loc[3], colourScale="red", projectOverlays=FALSE, alpha=0.5, prefix=prefix)
}
