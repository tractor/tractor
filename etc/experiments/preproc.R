require(tractor.fsl)

runExperiment <- function ()
{
    if (nArguments() == 0)
        session <- newSessionFromDirectory(".")
    else
        session <- newSessionFromDirectory(Arguments[1])
    
    targetDir <- session$getPreBedpostDirectory()

    interactive <- getWithDefault("Interactive", TRUE)
    fromScratch <- getWithDefault("FromScratch", FALSE)
    betIntensityThreshold <- getWithDefault("BetIntensityThreshold", 0.5)
    betVerticalGradient <- getWithDefault("BetVerticalGradient", 0)
    flipAxes <- getWithDefault("FlipGradientAxes", NULL, "integer")
    nFibres <- getWithDefault("NumberOfFibres", 2, errorIfInvalid=TRUE)
    howRunBedpost <- getWithDefault("HowRunBedpost", "auto")
    
    if (!fromScratch && session$isPreprocessed() && session$nFibres() == nFibres)
        output(OL$Info, "This session directory is already preprocessed")
    else try(
    {
        if (fromScratch || !file.exists(session$getWorkingDirectory()))
            createFilesForSession(session)
    
        if (!imageFileExists(file.path(targetDir, "nodif")))
            runEddyCorrectWithSession(session, ask=interactive)
    
        if (!imageFileExists(session$getImageFileNameByType("mask")))
        {
            runBetWithSession(session, betIntensityThreshold, betVerticalGradient, show=interactive)
        
            if (interactive)
            {
                runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn]")
                while (tolower(runBetAgain) == "y")
                {
                    output(OL$Info, "Previous intensity threshold was ", betIntensityThreshold, "; smaller values give larger brain outlines")
                    betIntensityThreshold <- as.numeric(output(OL$Question, "Intensity threshold? [0 to 1]"))
                    output(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                    betVerticalGradient <- as.numeric(output(OL$Question, "Vertical gradient? [-1 to 1]"))
                    runBetWithSession(session, betIntensityThreshold, betVerticalGradient, show=TRUE)
                    runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn]")
                }
            }
        }
    
        if (interactive && !imageFileExists(session$getImageFileNameByType("fa")))
        {
            runDtifitAgain <- output(OL$Question, "Run dtifit for diffusion tensor metrics? [yn]")
            while (tolower(runDtifitAgain) == "y")
            {
                if (is.null(flipAxes))
                {
                    ans <- output(OL$Question, "Flip diffusion gradient vectors along which axes? [123; Enter for none]")
                    flipAxes <- suppressWarnings(as.numeric(unlist(strsplit(ans,""))))
                }
                if (length(flipAxes[!is.na(flipAxes)]) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
                runDtifitWithSession(session)
                runDtifitAgain <- output(OL$Question, "Run dtifit again? [yn]")
                flipAxes <- NULL
            }
        }
        else if (!is.null(flipAxes))
            flipGradientVectorsForSession(session, flipAxes)
        
        runBedpostWithSession(session, nFibres, how=howRunBedpost, ask=interactive)
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
