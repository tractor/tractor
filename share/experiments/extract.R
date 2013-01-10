#@args image file, data value(s) to extract
#@desc Create a volume which is the same as the input volume, except that all voxels whose value is in the list of data values to extract are set to one, and all other voxels are given the value zero. Projection images can optionally be created with CreateImages:true. The base name of the output files is specified with the RegionName option.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("image file", "data value(s) to extract")
    image <- readImageFile(Arguments[1])
    values <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    
    createImages <- getConfigVariable("CreateImages", FALSE)
    overlayOnBrain <- getConfigVariable("OverlayOnBrain", FALSE)
    regionColour <- getConfigVariable("RegionColour", "green")
    regionName <- getConfigVariable("RegionName", "region")
    
    selectionFunction <- function (x)
    {
        dims <- dim(x)
        data <- ifelse(x %in% values, 1, 0)
        dim(data) <- dims
        return (data)
    }
    
    newImage <- newMriImageWithSimpleFunction(image, selectionFunction)
    writeImageFile(newImage, regionName)
    
    if (createImages)
    {
        if (overlayOnBrain && is.null(getFileNameForStandardImage("brain",errorIfMissing=FALSE)))
        {
            report(OL$Warning, "Cannot find standard brain volumes - no overlay possible")
            overlayOnBrain <- FALSE
        }
        
        if (!overlayOnBrain)
            createCombinedGraphics(list(newImage), "p", list(regionColour), device="png", prefix=regionName)
        else
        {
            brainImage <- getStandardImage("brain")
            if (!equivalent(newImage$getFieldOfView(), brainImage$getFieldOfView()))
                report(OL$Error, "The specified image is not in standard space")
            
            points <- which(newImage$getData()==1, arr.ind=TRUE)
            sliceLoc <- as.vector(apply(points, 2, median))
            sliceLoc <- round(sliceLoc * brainImage$getDimensions() / newImage$getDimensions())
            
            createCombinedGraphics(list(brainImage,newImage), c("s","p"), list(1,regionColour), sliceLoc=sliceLoc, device="png", alphaImage=list(NULL,newImage), prefix=regionName)
        }
    }
}
