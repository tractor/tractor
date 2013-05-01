#@args volume prefix
#@desc Create a group map by transforming a set of tract images into standard space and overlaying them. The colour scheme will indicate the number of sessions in which the tract passed through each voxel. The volume prefix given will usually have the form "TractName_session", to which session numbers will be appended by the script. Another prefix may be needed if the volumes were not created by "hnt-viz", "pnt-viz" or "pnt-prune".

library(tractor.reg)
library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    requireArguments("volume prefix")
    fileStem <- Arguments[1]
    
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    tractName <- getConfigVariable("TractName", NULL, "character")
    sessionNumbers <- getConfigVariable("SessionNumbers", NULL, "character")
    
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    baseThreshold <- getConfigVariable("ThresholdLevel", 0.01)
    thresholdMode <- getConfigVariable("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    binarise <- getConfigVariable("Binarise", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    useReferencePlanes <- getConfigVariable("UseReferencePlanes", TRUE)
    showReference <- getConfigVariable("ShowReferenceTract", FALSE)
    createColourBar <- getConfigVariable("CreateColourBar", FALSE)
    
    if (createImages && is.null(tractName))
        report(OL$Error, "Tract name must be specified if the CreateImages option is set")
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            report(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    finalImage <- NULL
    
    if (is.null(sessionNumbers))
        sessionNumbers <- seq_along(sessionList)
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    for (i in sessionNumbers)
    {
        report(OL$Info, "Current session is ", sessionList[i])
        
        imageFileName <- paste(fileStem, i, sep="")
        if (!imageFileExists(imageFileName))
        {
            report(OL$Warning, "No image available for session ", i)
            next
        }
        
        image <- readImageFile(imageFileName)
        session <- newSessionFromDirectory(sessionList[i])
        
        threshold <- baseThreshold * switch(thresholdMode, nothing=1, maximum=max(image,na.rm=TRUE), minimum=min(image,na.rm=TRUE))
        
        transformedImage <- transformImageToSpace(image, session, "mni", oldSpace="diffusion")
        if (binarise)
            thresholdedImage <- newMriImageWithSimpleFunction(transformedImage, function(x) ifelse(x>=threshold,1,0))
        else
            thresholdedImage <- newMriImageByThresholding(transformedImage, threshold)
        
        if (is.null(finalImage))
            finalImage <- thresholdedImage
        else
            finalImage <- newMriImageWithBinaryFunction(finalImage, thresholdedImage, "+")
    }
    
    if (is.null(finalImage))
        report(OL$Error, "No image information available")
    
    outputName <- paste(tractName, "_group_map", sep="")
    writeImageFile(finalImage, outputName)
    
    if (createImages)
    {
        if (showReference || useReferencePlanes)
        {
            refTractFileName <- getFileNameForNTResource("reference", "hnt", list(tractName=tractName), expectExists=TRUE)
            reference <- deserialiseReferenceObject(refTractFileName)
        }
        if (showReference && !is(reference,"FieldTract"))
        {
            report(OL$Warning, "No HNT reference tract of the specified name is available - the reference tract will not be shown")
            showReference <- FALSE
        }
        
        brainImage <- getStandardImage("white")
        
        if (binarise)
            alphaImage <- newMriImageWithSimpleFunction(finalImage, function(x) ifelse(x>0,1,0))
        else
            alphaImage <- newMriImageWithSimpleFunction(finalImage, function(x) ifelse(x>0,log(x),0))
        
        if (useReferencePlanes)
        {
            if (reference$getSeedUnit() == "vox")
                seedLoc <- reference$getStandardSpaceSeedPoint()
            else
                seedLoc <- transformWorldToVoxel(reference$getStandardSpaceSeedPoint(), brainImage$getMetadata())
        }
        else
        {
            seedLoc <- which(finalImage$getData()==max(finalImage), arr.ind=TRUE)
            seedLoc <- apply(seedLoc, 2, median)
        }
        
        if (showReference)
        {
            referenceImage <- newMriImageWithSimpleFunction(reference$getImage(), function(x) ifelse(x>0,1,0))
            createCombinedGraphics(list(brainImage,finalImage,referenceImage), c("s","p","p"), list(1,3,"green"), sliceLoc=round(seedLoc), device="png", alphaImage=list(NULL,alphaImage,referenceImage), prefix=outputName, windowLimits=list(NULL,windowLimits,NULL))
        }
        else
            createCombinedGraphics(list(brainImage,finalImage), c("s","p"), list(1,3), sliceLoc=round(seedLoc), device="png", alphaImage=list(NULL,alphaImage), prefix=outputName, windowLimits=list(NULL,windowLimits))
    }
    
    if (createColourBar)
    {
        data <- matrix(1:100, nrow=100, ncol=20)
        tractor.base:::writePng(data, colourScale=3, fileName="colourbar.png")
    }
}
