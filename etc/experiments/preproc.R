#@args [session directory]
#@desc Runs the standard FSL-FDT preprocessing pipeline on the specified session
#@desc directory (or "." if none is specified). This pipeline consists of five stages:
#@desc (1) convert DICOM files into a 4D Analyze/NIfTI volume; (2) correct the data
#@desc set for eddy current induced distortions; (3) create a mask to extract only
#@desc brain voxels; (4, optional) calculate diffusion tensor characteristics such as
#@desc principal eigenvectors and FA values; (5) run BEDPOSTX. The last stage takes
#@desc many hours. If the pipeline was previously partly completed, the script will
#@desc resume it where appropriate. (Starting from the beginning can be forced by
#@desc specifying SkipCompletedStages:false.) The script asks the user about each stage
#@desc unless Interactive:false is given. Note that BEDPOSTX will be run using a 2
#@desc fibre model at each voxel by default - this is changed with the NumberOfFibres
#@desc option.
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
    stages <- getWithDefault("RunStages", "12345")
    skipCompleted <- getWithDefault("SkipCompletedStages", TRUE)
    dicomDir <- getWithDefault("DicomDirectory", NULL, "character")
    betIntensityThreshold <- getWithDefault("BetIntensityThreshold", 0.5)
    betVerticalGradient <- getWithDefault("BetVerticalGradient", 0)
    flipAxes <- getWithDefault("FlipGradientAxes", NULL, "character")
    nFibres <- getWithDefault("NumberOfFibres", NULL, "integer")
    howRunBedpost <- getWithDefault("HowRunBedpost", "auto")
    
    if (interactive && getOption("tractorOutputLevel") > OL$Info)
        setOutputLevel(OL$Info)
    
    if (!is.null(flipAxes))
    {
        flipAxes <- suppressWarnings(as.numeric(unlist(strsplit(flipAxes, ""))))
        flipAxes <- flipAxes[!is.na(flipAxes)]
    }
    
    runStages <- 1:5 %in% suppressWarnings(as.numeric(unlist(strsplit(stages, ""))))
    if (all(!runStages))
        output(OL$Info, "Nothing to do")
    
    if (skipCompleted && session$isPreprocessed() && (is.null(nFibres) || session$nFibres() == nFibres))
        output(OL$Info, "This session directory is already preprocessed")
    else try(
    {
        if (runStages[1] && (!skipCompleted || !imageFileExists(file.path(targetDir,"basic"))))
        {
            createFilesForSession(session, dicomDir, overwriteQuietly=(!interactive))
            reportFlags()
        }
    
        if (runStages[2] && (!skipCompleted || !imageFileExists(file.path(targetDir,"nodif"))))
            runEddyCorrectWithSession(session, ask=interactive)
    
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
                        betIntensityThreshold <- as.numeric(output(OL$Question, "Intensity threshold? [0 to 1]"))
                        output(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                        betVerticalGradient <- as.numeric(output(OL$Question, "Vertical gradient? [-1 to 1]"))
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
                            flipAxes <- suppressWarnings(as.numeric(unlist(strsplit(ans,""))))
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
    
    returnValue <- list(workingDirectoryExists=file.exists(session$getWorkingDirectory()),
                        eddyCorrectCompleted=imageFileExists(file.path(targetDir, "nodif")),
                        betCompleted=imageFileExists(session$getImageFileNameByType("mask")),
                        dtifitCompleted=imageFileExists(session$getImageFileNameByType("fa")),
                        allPreprocessingCompleted=session$isPreprocessed(),
                        numberOfFibres=session$nFibres())
    if (session$usesOldBedpost())
        returnValue <- c(returnValue, list(usesOldBedpost=TRUE))
    
    invisible (returnValue)
}
