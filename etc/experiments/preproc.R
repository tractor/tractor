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
    nFibres <- getWithDefault("NumberOfFibres", 2, errorIfInvalid=TRUE)
    howRunBedpost <- getWithDefault("HowRunBedpost", "auto")
    
    if (session$isPreprocessed())
        output(OL$Info, "This session directory is already preprocessed")
    else
    {
        if (fromScratch || !file.exists(session$getWorkingDirectory()))
            try(createFilesForSession(session))
    
        if (!imageFileExists(file.path(targetDir, "nodif")))
            try(runEddyCorrectWithSession(session, ask=interactive))
    
        if (!imageFileExists(session$getImageFileNameByType("mask")))
        {
            try(runBetWithSession(session, betIntensityThreshold, betVerticalGradient, show=interactive))
        
            if (interactive)
            {
                runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn]")
                while (tolower(runBetAgain) == "y")
                {
                    output(OL$Info, "Previous intensity threshold was ", betIntensityThreshold, "; smaller values give larger brain outlines")
                    betIntensityThreshold <- as.numeric(output(OL$Question, "Intensity threshold? [0 to 1]"))
                    output(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                    betVerticalGradient <- as.numeric(output(OL$Question, "Vertical gradient? [-1 to 1]"))
                    try(runBetWithSession(session, betIntensityThreshold, betVerticalGradient, show=TRUE))
                    runBetAgain <- output(OL$Question, "Run brain extraction tool again? [yn]")
                }
            }
        }
    
        if (interactive && !imageFileExists(session$getImageFileNameByType("fa")))
        {
            ans <- output(OL$Question, "Run dtifit for diffusion tensor metrics? [yn]")
            if (tolower(ans) == "y")
                try(runDtifitWithSession(session))
        }
        
        try(runBedpostWithSession(session, nFibres, how=howRunBedpost, ask=interactive))
    }
    
    returnValue <- list(workingDirectoryExists=file.exists(session$getWorkingDirectory()),
                        eddyCorrectCompleted=imageFileExists(file.path(targetDir, "nodif")),
                        betCompleted=imageFileExists(session$getImageFileNameByType("mask")),
                        dtifitCompleted=imageFileExists(session$getImageFileNameByType("fa")),
                        allPreprocessingCompleted=session$isPreprocessed(),
                        numberOfFibres=session$nFibres())
    invisible (returnValue)
}
