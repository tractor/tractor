#@args volume prefix
#@desc Create a group map by transforming a set of tract images into standard space and overlaying them. The colour scheme will indicate the number of sessions in which the tract passed through each voxel. The volume prefix given will usually have the form "TractName_session", to which session numbers will be appended by the script. Another prefix may be needed if the volumes were not created by "hnt-viz", "pnt-viz" or "pnt-prune".

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("volume prefix")
    fileStem <- Arguments[1]
    
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    tractName <- getWithDefault("TractName", NULL, "character")
    sessionNumbers <- getWithDefault("SessionNumbers", NULL, "character")
    
    windowLimits <- getWithDefault("WindowLimits", NULL, "character")
    baseThreshold <- getWithDefault("ThresholdLevel", 0.01)
    thresholdMode <- getWithDefault("ThresholdRelativeTo", "nothing", validValues=c("nothing","maximum","minimum"))
    binarise <- getWithDefault("Binarise", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    showReference <- getWithDefault("ShowReferenceTract", FALSE)
    
    if (createImages && is.null(tractName))
        output(OL$Error, "Tract name must be specified if the CreateImages option is set")
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            output(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    finalImage <- NULL
    
    if (is.null(sessionNumbers))
        sessionNumbers <- seq_along(sessionList)
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    for (i in sessionNumbers)
    {
        output(OL$Info, "Current session is ", sessionList[i])
        
        imageFileName <- paste(fileStem, i, sep="")
        if (!imageFileExists(imageFileName))
        {
            output(OL$Warning, "No image available for session ", i)
            next
        }
        
        image <- newMriImageFromFile(imageFileName)
        session <- newSessionFromDirectory(sessionList[i])
        
        threshold <- baseThreshold * switch(thresholdMode, nothing=1, maximum=max(image,na.rm=TRUE), minimum=min(image,na.rm=TRUE))
        
        transformedImage <- transformStandardSpaceImage(session, image, toStandard=TRUE)
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
        output(OL$Error, "No image information available")
    
    outputName <- paste(tractName, "_group_map", sep="")
    writeMriImageToFile(finalImage, outputName)
    
    if (createImages)
    {
        refTractFileName <- getFileNameForNTResource("reference", "hnt", list(tractName=tractName), expectExists=TRUE)
        reference <- deserialiseReferenceTract(fileName)
        if (showReference && !isFieldTract(reference))
        {
            output(OL$Warning, "No HNT reference tract of the specified name is available - the reference tract will not be shown")
            showReference <- FALSE
        }
        
        brainImage <- getStandardImage("white")
        
        if (binarise)
            alphaImage <- newMriImageWithSimpleFunction(finalImage, function(x) ifelse(x>0,1,0))
        else
            alphaImage <- newMriImageWithSimpleFunction(finalImage, function(x) ifelse(x>0,log(x),0))
        
        if (reference$getSeedUnit() == "vox")
            seedLoc <- reference$getStandardSpaceSeedPoint()
        else
            seedLoc <- transformWorldToRVoxel(reference$getStandardSpaceSeedPoint(), brainImage$getMetadata(), useOrigin=TRUE)
        
        if (showReference)
        {
            referenceImage <- newMriImageWithSimpleFunction(reference$getImage(), function(x) ifelse(x>0,1,0))
            createCombinedGraphics(list(brainImage,finalImage,referenceImage), c("s","p","p"), list(1,3,"green"), sliceLoc=seedLoc, device="png", alphaImage=list(NULL,alphaImage,referenceImage), prefix=outputName, windowLimits=list(NULL,windowLimits,NULL))
        }
        else
            createCombinedGraphics(list(brainImage,finalImage), c("s","p"), list(1,3), sliceLoc=seedLoc, device="png", alphaImage=list(NULL,alphaImage), prefix=outputName, windowLimits=list(NULL,windowLimits))
    }
}
