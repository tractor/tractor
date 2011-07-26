#@args [session directory]
#@desc Runs the standard FSL-FDT preprocessing pipeline on the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI volume; (2) correct the data set for eddy current induced distortions; (3) create a mask to extract only brain voxels; (4, optional) calculate diffusion tensor characteristics such as principal eigenvectors and FA values. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) The script asks the user about each stage unless Interactive:false is given.
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    if (nArguments() == 0)
        session <- newSessionFromDirectory(".")
    else
        session <- newSessionFromDirectory(Arguments[1])
    
    interactive <- getConfigVariable("Interactive", TRUE)
    stages <- getConfigVariable("RunStages", "1-4")
    skipCompleted <- getConfigVariable("SkipCompletedStages", TRUE)
    dicomDir <- getConfigVariable("DicomDirectory", NULL, "character")
    useGradientCache <- getConfigVariable("UseGradientCache", "second", validValues=c("first","second","never"))
    forceCacheUpdate <- getConfigVariable("ForceGradientCacheUpdate", FALSE)
    maskingMethod <- getConfigVariable("MaskingMethod", "bet", validValues=c("bet","kmeans","fill"))
    betIntensityThreshold <- getConfigVariable("BetIntensityThreshold", 0.3)
    betVerticalGradient <- getConfigVariable("BetVerticalGradient", 0)
    flipAxes <- getConfigVariable("FlipGradientAxes", NULL, "character")
    
    if (interactive && getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    if (!is.null(flipAxes))
    {
        flipAxes <- splitAndConvertString(flipAxes, "", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        flipAxes <- flipAxes[!is.na(flipAxes)]
    }
    
    stages <- splitAndConvertString(stages, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    runStages <- 1:4 %in% stages
    if (all(!runStages))
        report(OL$Info, "Nothing to do")
    
    try({
        if (runStages[1] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("rawdata","diffusion"))))
        {
            createFilesForSession(session, dicomDir, overwriteQuietly=(!interactive))
            if (useGradientCache == "first" || (useGradientCache == "second" && !gradientDirectionsAvailableForSession(session)))
            {
                gradientSet <- checkGradientCacheForSession(session)
                if (!is.null(gradientSet))
                {
                    report(OL$Info, "Gradient cache hit - using stored gradient scheme")
                    scheme <- newSimpleDiffusionSchemeWithDirections(t(gradientSet[,1:3]), gradientSet[,4])
                    writeSimpleDiffusionSchemeForSession(session, scheme)
                }
            }
            
            if (!gradientDirectionsAvailableForSession(session))
                report(OL$Warning, "Diffusion gradient information not available - you need to create bvals and bvecs files manually")
            
            reportFlags()
        }
    
        if (runStages[2] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("refb0","diffusion"))))
        {
            if (!is.null(flipAxes) && length(flipAxes) > 0)
                flipGradientVectorsForSession(session, flipAxes)
            
            if (isTRUE(updateGradientCacheFromSession(session, forceCacheUpdate)))
                report(OL$Info, "Gradient directions inserted into cache for future reference")
            runEddyCorrectWithSession(session, ask=interactive)
        }
    
        if (runStages[3] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("mask","diffusion"))))
        {
            if (maskingMethod == "bet")
            {
                runBetWithSession(session, betIntensityThreshold, betVerticalGradient)

                if (interactive)
                {
                    runBetAgain <- ask("Run brain extraction tool again? [yn; s to show the mask in fslview]")
                    while (tolower(runBetAgain) %in% c("y","s"))
                    {
                        if (tolower(runBetAgain) == "s")
                            runBetWithSession(session, showOnly=TRUE)
                        else
                        {
                            report(OL$Info, "Previous intensity threshold was ", betIntensityThreshold, "; smaller values give larger brain outlines")
                            tempValue <- ask("Intensity threshold? [0 to 1; Enter for same as before]")
                            if (tempValue != "")
                                betIntensityThreshold <- as.numeric(tempValue)

                            report(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                            tempValue <- ask("Vertical gradient? [-1 to 1; Enter for same as before]")
                            if (tempValue != "")
                                betVerticalGradient <- as.numeric(tempValue)

                            runBetWithSession(session, betIntensityThreshold, betVerticalGradient)
                        }
                        runBetAgain <- ask("Run brain extraction tool again? [yn; s to show the mask in fslview]")
                    }
                }
            }
            else
                createMaskImageForSession(session, maskingMethod)
        }
    
        if (runStages[4] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("fa","diffusion"))))
        {
            runDtifitWithSession(session)
            
            if (interactive)
            {
                repeat
                {
                    runDtifitAgain <- tolower(ask("Run dtifit again? [yn; s to show principal directions in fslview]"))
                    
                    if (runDtifitAgain == "n")
                        break
                    else if (runDtifitAgain == "s")
                        runDtifitWithSession(session, showOnly=TRUE)
                    else if (runDtifitAgain == "y")
                    {
                        ans <- ask("Flip diffusion gradient vectors along which axes? [123; Enter for none]")
                        flipAxes <- splitAndConvertString(ans, "", "integer", fixed=TRUE, allowRanges=FALSE)
                        
                        if (length(flipAxes[!is.na(flipAxes)]) > 0)
                            flipGradientVectorsForSession(session, flipAxes)
                        runDtifitWithSession(session)
                    }
                }
            }
        }
    })
    
    invisible (NULL)
}
