#@args [session directory]
#@desc Runs the standard FSL-FDT preprocessing pipeline on the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI/MGH volume; (2) correct the data set for eddy current induced distortions; (3) create a mask to extract only brain voxels; (4, optional) calculate diffusion tensor characteristics such as principal eigenvectors and FA values. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) The script asks the user about each stage unless Interactive:false is given.
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
        flipAxes <- splitAndConvertString(flipAxes, "", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        flipAxes <- flipAxes[!is.na(flipAxes)]
    }
    
    stages <- splitAndConvertString(stages, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    runStages <- 1:4 %in% stages
    if (all(!runStages))
        report(OL$Info, "Nothing to do")
    
    stagesComplete <- c(imageFileExists(session$getImageFileNameByType("rawdata","diffusion")),
                        imageFileExists(session$getImageFileNameByType("refb0","diffusion")),
                        imageFileExists(session$getImageFileNameByType("data","diffusion")),
                        imageFileExists(session$getImageFileNameByType("mask","diffusion")))

    if (statusOnly)
        printLabelledValues("Stages completed", implode(which(stagesComplete),", "))
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
            else if (dicomDir %!~% "^/")
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
            
            writeLines(choice, file.path(session$getDirectory("diffusion"),"refb0-index.txt"))
            
            report(OL$Info, "Extracting reference volume")
            data <- session$getImageByType("data","diffusion")
            refVolume <- newMriImageByExtraction(data, 4, choice)
            writeMriImageToFile(refVolume, session$getImageFileNameByType("refb0"))
        }
        
        if (runStages[3] && (!skipCompleted || !stagesComplete[3]))
        {
            refVolume <- as.integer(readLines(file.path(session$getDirectory("diffusion"),"refb0-index.txt")))
            runEddyCorrectWithSession(session, refVolume)
        }
        
        if (runStages[4] && (!skipCompleted || !stagesComplete[4]))
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
                            showImagesInFslview(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("mask","diffusion"))
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
    }
    
    invisible (NULL)
}
