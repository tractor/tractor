#@args [session directory]
#@desc Runs the standard FSL-FDT preprocessing pipeline on the specified session directory (or "." if none is specified). This pipeline consists of five stages: (1) convert DICOM files into a 4D Analyze/NIfTI volume; (2) correct the data set for eddy current induced distortions; (3) create a mask to extract only brain voxels; (4, optional) calculate diffusion tensor characteristics such as principal eigenvectors and FA values; (5) run BEDPOSTX. The last stage takes many hours. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) The script asks the user about each stage unless Interactive:false is given. Note that BEDPOSTX will be run using a 2 fibre model at each voxel by default - this is changed with the NumberOfFibres option. Diffusion gradient directions will be rotated to compensate for the eddy current correction process as part of stage 2 if RotateGradients:true is given.
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    if (nArguments() == 0)
        session <- newSessionFromDirectory(".")
    else
        session <- newSessionFromDirectory(Arguments[1])
    
    targetDir <- session$getPreBedpostDirectory()

    interactive <- getWithDefault("Interactive", TRUE)
    stages <- getWithDefault("RunStages", "1-5")
    skipCompleted <- getWithDefault("SkipCompletedStages", TRUE)
    dicomDir <- getWithDefault("DicomDirectory", NULL, "character")
    useGradientCache <- getWithDefault("UseGradientCache", "second", validValues=c("first","second","never"))
    rotateGradients <- getWithDefault("RotateGradients", FALSE)
    betIntensityThreshold <- getWithDefault("BetIntensityThreshold", 0.5)
    betVerticalGradient <- getWithDefault("BetVerticalGradient", 0)
    flipAxes <- getWithDefault("FlipGradientAxes", NULL, "character")
    nFibres <- getWithDefault("NumberOfFibres", NULL, "integer")
    howRunBedpost <- getWithDefault("HowRunBedpost", "fg", validValues=c("fg","bg","screen"))
    
    if (interactive && getOption("tractorOutputLevel") > OL$Info)
        setOutputLevel(OL$Info)
    
    if (!is.null(flipAxes))
    {
        flipAxes <- splitAndConvertString(flipAxes, "", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        flipAxes <- flipAxes[!is.na(flipAxes)]
    }
    
    stages <- splitAndConvertString(stages, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    # Handle old format RunStages values
    if (any(stages > 5))
        stages <- splitAndConvertString(as.character(stages), "", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    runStages <- 1:5 %in% stages
    if (all(!runStages))
        output(OL$Info, "Nothing to do")
    
    if (skipCompleted && session$isPreprocessed() && (is.null(nFibres) || session$nFibres() == nFibres))
        output(OL$Info, "This session directory is already preprocessed")
    else try(
    {
        if (runStages[1] && (!skipCompleted || !imageFileExists(file.path(targetDir,"basic"))))
        {
            createFilesForSession(session, dicomDir, overwriteQuietly=(!interactive))
            if (useGradientCache == "first" || (useGradientCache == "second" && !gradientDirectionsAvailableForSession(session)))
            {
                gradientSet <- checkGradientCacheForSession(session)
                if (!is.null(gradientSet))
                {
                    output(OL$Info, "Gradient cache hit - using stored gradient scheme")
                    write.table(t(gradientSet[,1:3]), file.path(targetDir,"bvecs"), row.names=FALSE, col.names=FALSE)
                    write.table(t(gradientSet[,4,drop=FALSE]), file.path(targetDir,"bvals"), row.names=FALSE, col.names=FALSE)
                }
            }
            
            if (!gradientDirectionsAvailableForSession(session))
                output(OL$Warning, "Diffusion gradient information not available - you need to create bvals and bvecs files manually")
            
            reportFlags()
        }
    
        if (runStages[2] && (!skipCompleted || !imageFileExists(file.path(targetDir,"nodif"))))
        {
            if (isTRUE(updateGradientCacheFromSession(session)))
                output(OL$Info, "Gradient directions inserted into cache for future reference")
            runEddyCorrectWithSession(session, ask=interactive)
            
            if (rotateGradients)
                rotateGradientVectorsForSession(session)
        }
    
        if (runStages[3] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("mask"))))
        {
            runBetWithSession(session, betIntensityThreshold, betVerticalGradient)
        
            if (interactive)
            {
                runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn; s to show the mask in fslview]")
                while (tolower(runBetAgain) %in% c("y","s"))
                {
                    if (tolower(runBetAgain) == "s")
                        runBetWithSession(session, showOnly=TRUE)
                    else
                    {
                        output(OL$Info, "Previous intensity threshold was ", betIntensityThreshold, "; smaller values give larger brain outlines")
                        tempValue <- output(OL$Question, "Intensity threshold? [0 to 1; Enter for same as before]")
                        if (tempValue != "")
                            betIntensityThreshold <- as.numeric(tempValue)
                        
                        output(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                        tempValue <- output(OL$Question, "Vertical gradient? [-1 to 1; Enter for same as before]")
                        if (tempValue != "")
                            betVerticalGradient <- as.numeric(tempValue)
                        
                        runBetWithSession(session, betIntensityThreshold, betVerticalGradient)
                    }
                    runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn; s to show the mask in fslview]")
                }
            }
        }
    
        if (runStages[4] && (!skipCompleted || !imageFileExists(session$getImageFileNameByType("fa"))))
        {
            if (interactive)
            {
                runDtifitAgain <- "y"
                while (tolower(runDtifitAgain) %in% c("y","s"))
                {
                    if (tolower(runDtifitAgain) == "s")
                        runDtifitWithSession(session, showOnly=TRUE)
                    else
                    {
                        if (is.null(flipAxes))
                        {
                            ans <- output(OL$Question, "Flip diffusion gradient vectors along which axes? [123; Enter for none]")
                            flipAxes <- splitAndConvertString(ans, "", "integer", fixed=TRUE, allowRanges=FALSE)
                        }
                        if (length(flipAxes[!is.na(flipAxes)]) > 0)
                            flipGradientVectorsForSession(session, flipAxes)
                        runDtifitWithSession(session)
                    }
                    
                    runDtifitAgain <- output(OL$Question, "Run dtifit again? [yn; s to show principal directions in fslview]")
                    flipAxes <- NULL
                }
            }
            else
            {
                if (!is.null(flipAxes) && length(flipAxes[!is.na(flipAxes)]) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
                runDtifitWithSession(session)
            }
        }
        else if (!is.null(flipAxes) && length(flipAxes) > 0)
            flipGradientVectorsForSession(session, flipAxes)
        
        if (runStages[5])
        {
            if (is.null(nFibres))
                nFibres <- 2
            runBedpostWithSession(session, nFibres, how=howRunBedpost, ask=interactive)
        }
    } )
    
    invisible (NULL)
}
