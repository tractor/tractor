#@args image file, data value(s) to extract

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
        if (!overlayOnBrain || is.null(.StandardBrainPath))
            createCombinedGraphics(list(newImage), "p", regionColour, device="png", prefix=regionName)
        else
        {
            brainImage <- newMriImageFromFile(file.path(.StandardBrainPath,"avg152T1_brain"))
            if (!equivalent(newImage$getFieldOfView(), brainImage$getFieldOfView()))
                output(OL$Error, "The specified image is not in standard space")
            
            points <- which(newImage$getData()==1, arr.ind=TRUE)
            sliceLoc <- as.vector(apply(points, 2, median))
            sliceLoc <- round(sliceLoc * brainImage$getDimensions() / newImage$getDimensions())
            
            createCombinedGraphics(list(brainImage,newImage), c("s","p"), list(1,regionColour), sliceLoc=sliceLoc, device="png", alphaImage=list(NULL,newImage), prefix=regionName)
        }
    }
}
