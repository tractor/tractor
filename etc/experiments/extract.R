#@args image file, data value(s) to extract
#@desc Create a volume which is the same as the input volume, except that all voxels
#@desc whose value is not in the list of data values to extract have their values set
#@desc to zero. Projection images can optionally be created with CreateImages:true.
#@desc The base name of the output files is specified with the RegionName option.

suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("image file", "data value(s) to extract")
    image <- newMriImageFromFile(Arguments[1])
    values <- as.numeric(unlist(strsplit(Arguments[-1], ",")))
    
    createImages <- getWithDefault("CreateImages", FALSE)
    overlayOnBrain <- getWithDefault("OverlayOnBrain", FALSE)
    regionColour <- getWithDefault("RegionColour", "green")
    regionName <- getWithDefault("RegionName", "region")
    
    selectionFunction <- function (x)
    {
        dims <- dim(x)
        data <- ifelse(x %in% values, 1, 0)
        dim(data) <- dims
        return (data)
    }
    
    newImage <- newMriImageWithSimpleFunction(image, selectionFunction)
    writeMriImageToFile(newImage, regionName)
    
    if (createImages)
    {
        if (overlayOnBrain && is.null(getFileNameForStandardImage("brain",errorIfMissing=FALSE)))
        {
            output(OL$Warning, "Cannot find standard brain volumes - no overlay possible")
            overlayOnBrain <- FALSE
        }
        
        if (!overlayOnBrain)
            createCombinedGraphics(list(newImage), "p", regionColour, device="png", prefix=regionName)
        else
        {
            brainImage <- getStandardImage("brain")
            if (!equivalent(newImage$getFieldOfView(), brainImage$getFieldOfView()))
                output(OL$Error, "The specified image is not in standard space")
            
            points <- which(newImage$getData()==1, arr.ind=TRUE)
            sliceLoc <- as.vector(apply(points, 2, median))
            sliceLoc <- round(sliceLoc * brainImage$getDimensions() / newImage$getDimensions())
            
            createCombinedGraphics(list(brainImage,newImage), c("s","p"), list(1,regionColour), sliceLoc=sliceLoc, device="png", alphaImage=list(NULL,newImage), prefix=regionName)
        }
    }
}
