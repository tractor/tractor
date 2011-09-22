#@args [session directory]
#@desc Run a standard preprocessing pipeline on the diffusion data in the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI/MGH volume; (2) identify a volume with little or no diffusion weighting to use as an anatomical reference; (3) create a mask to identify voxels which are within the brain; (4) correct the data set for eddy current induced distortions. Stage 4 is currently performed using FSL "eddy_correct", so FSL must be installed for this stage. Stage 3 can use FSL's brain extraction tool for accuracy, but the default is to use a k-means approach, which is quicker, has no parameters, and may be more successful for nonbrain data. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) Giving StatusOnly:true will report which stages have been run. The script asks the user about each stage unless Interactive:false is given.
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    statusOnly <- getConfigVariable("StatusOnly", FALSE)
    interactive <- getConfigVariable("Interactive", TRUE)
    stages <- getConfigVariable("RunStages", "1-4")
    skipCompleted <- getConfigVariable("SkipCompletedStages", TRUE)
    dicomDir <- getConfigVariable("DicomDirectory", NULL, "character")
    useGradientCache <- getConfigVariable("UseGradientCache", "second", validValues=c("first","second","never"))
    flipAxes <- getConfigVariable("FlipGradientAxes", NULL, "character")
    maskingMethod <- getConfigVariable("MaskingMethod", "kmeans", validValues=c("bet","kmeans","fill"))
    betIntensityThreshold <- getConfigVariable("BetIntensityThreshold", 0.3)
    betVerticalGradient <- getConfigVariable("BetVerticalGradient", 0)
    
    if ((interactive || statusOnly) && getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    if (!is.null(flipAxes))
    {
        if (flipAxes %~% "\\d")
            report(OL$Warning, "The \"FlipGradientAxes\" option should be specified as a comma-separated list of axis labels, as in \"x,y\"")
        flipAxes <- which(letters[24:26] %in% splitAndConvertString(flipAxes, ",", fixed=TRUE))
    }
    
    stages <- splitAndConvertString(stages, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    runStages <- 1:4 %in% stages
    if (all(!runStages))
        report(OL$Info, "Nothing to do")
    
    stagesComplete <- c(imageFileExists(session$getImageFileNameByType("rawdata","diffusion")),
                        imageFileExists(session$getImageFileNameByType("refb0","diffusion")),
                        imageFileExists(session$getImageFileNameByType("mask","diffusion")),
                        imageFileExists(session$getImageFileNameByType("data","diffusion")))

    if (statusOnly)
        printLabelledValues(c("Session directory","Stages completed"), c(session$getDirectory(),implode(which(stagesComplete),", ")))
    else
    {
        if (runStages[1] && (!skipCompleted || !stagesComplete[1]))
        {
            workingDir <- session$getDirectory("root")
            if (file.exists(workingDir))
            {
                if (interactive)
                {
                    ans <- ask("Internal directory ", workingDir, " exists. This operation will DESTROY it. Continue? [yn]")
                    if (tolower(ans) != "y")
                        return (invisible(NULL))
                }
                
                unlink(workingDir, recursive=TRUE)
            }

            if (is.null(dicomDir))
                dicomDir <- session$getDirectory()
            else if (dicomDir %!~% "^([A-Za-z]:)?/")
                dicomDir <- file.path(session$getDirectory(), dicomDir)
            dicomDir <- gsub("//+", "/", dicomDir, perl=TRUE)

            info <- newMriImageFromDicomDirectory(dicomDir, readDiffusionParams=TRUE)

            session$getDirectory("diffusion", createIfMissing=TRUE)
            writeMriImageToFile(info$image, session$getImageFileNameByType("rawdata","diffusion"))
            print(info$image)

            seriesDescriptions <- implode(gsub("\\W","",info$seriesDescriptions,perl=TRUE), ",")
            writeLines(seriesDescriptions, file.path(session$getDirectory("diffusion"),"descriptions.txt"))

            if (all(!is.na(info$bValues)) && all(!is.na(info$bVectors)))
            {
                scheme <- newSimpleDiffusionSchemeWithDirections(info$bVectors, info$bValues)
                writeSimpleDiffusionSchemeForSession(session, scheme)
            }
            
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
            else
            {
                if (!is.null(flipAxes) && length(flipAxes) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
                if (isTRUE(updateGradientCacheFromSession(session)))
                    report(OL$Info, "Gradient directions inserted into cache for future reference")
            }
        }
    
        if (runStages[2] && (!skipCompleted || !stagesComplete[2]))
        {
            scheme <- newSimpleDiffusionSchemeFromSession(session)
            if (is.null(scheme))
                report(OL$Error, "No b-value or gradient direction information is available")

            schemeComponents <- scheme$expandComponents()
            minBValue <- min(scheme$getBValues())

            zeroes <- which(schemeComponents$bValues == minBValue)
            if (length(zeroes) == 1)
            {
                choice <- zeroes
                report(OL$Info, "Volume ", choice, " is the only T2-weighted (b=", minBValue, ") volume in the data set")
            }
            else if (!interactive)
            {
                choice <- zeroes[1]
                report(OL$Info, "Using volume ", choice, " with b=", minBValue, " as the reference volume")
            }
            else
            {
                report(OL$Info, "Volumes ", implode(zeroes,sep=", ",finalSep=" and "), " are T2-weighted (b=", minBValue, ")")
                choice <- -1

                while (!(choice %in% zeroes))
                {
                    choice <- ask("Use which one as the reference [s to show in fslview]?")
                    if (tolower(choice) == "s")
                        showImagesInFslview(session$getImageByType("rawdata","diffusion"), writeToAnalyzeFirst=TRUE)
                    else
                        choice <- as.integer(choice)
                }
            }
            
            writeLines(as.character(choice), file.path(session$getDirectory("diffusion"),"refb0-index.txt"))
            
            report(OL$Info, "Extracting reference volume")
            data <- session$getImageByType("rawdata", "diffusion")
            refVolume <- newMriImageByExtraction(data, 4, choice)
            writeMriImageToFile(refVolume, session$getImageFileNameByType("refb0"))
        }
        
        if (runStages[3] && (!skipCompleted || !stagesComplete[3]))
        {
            if (maskingMethod == "bet")
            {
                runBetWithSession(session, betIntensityThreshold, betVerticalGradient)

                if (interactive)
                {
                    runBetAgain <- ask("Is the brain extraction satisfactory? [yn; s to show the mask in fslview]")
                    while (tolower(runBetAgain) %in% c("n","s"))
                    {
                        if (tolower(runBetAgain) == "s")
                            showImagesInFslview(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("mask","diffusion"), writeToAnalyzeFirst=TRUE)
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
                        runBetAgain <- ask("Is the brain extraction satisfactory? [yn; s to show the mask in fslview]")
                    }
                }
            }
            else
                createMaskImageForSession(session, maskingMethod)
        }
        
        if (runStages[4] && (!skipCompleted || !stagesComplete[4]))
        {
            refVolume <- as.integer(readLines(file.path(session$getDirectory("diffusion"),"refb0-index.txt")))
            runEddyCorrectWithSession(session, refVolume)
        }
    }
    
    invisible (NULL)
}
