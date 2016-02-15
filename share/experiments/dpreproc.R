#@args [session directory]
#@desc Run a standard preprocessing pipeline on the diffusion data in the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI/MGH volume; (2) identify a volume with little or no diffusion weighting to use as an anatomical reference; (3) create a mask to identify voxels which are within the brain; (4) correct the data set for eddy current induced distortions. Stage 4 is currently performed using FSL "eddy_correct" by default, but TractoR's native NiftyReg back-end can be used instead. Stage 3 can use FSL's brain extraction tool for accuracy, but the default is to use a k-means approach, which is quicker, and may be more successful for nonbrain data. (The KMeansClusters option can be increased if the mask does not cover the whole region of interest.) If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) Giving StatusOnly:true will report which stages have been run. The script asks the user about each stage unless Interactive:false is given.
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    statusOnly <- getConfigVariable("StatusOnly", FALSE)
    interactive <- getConfigVariable("Interactive", TRUE)
    stages <- getConfigVariable("RunStages", "1-4")
    skipCompleted <- getConfigVariable("SkipCompletedStages", TRUE)
    dicomDir <- getConfigVariable("DicomDirectory", NULL, "character")
    useGradientCache <- getConfigVariable("UseGradientCache", "second", validValues=c("first","second","never"))
    flipAxes <- getConfigVariable("FlipGradientAxes", NULL, "character")
    maskingMethod <- getConfigVariable("MaskingMethod", "kmeans", validValues=c("bet","kmeans","fill"))
    nClusters <- getConfigVariable("KMeansClusters", 2, "integer")
    betIntensityThreshold <- getConfigVariable("BetIntensityThreshold", 0.3)
    betVerticalGradient <- getConfigVariable("BetVerticalGradient", 0)
    eddyCorrectMethod <- getConfigVariable("EddyCorrectionMethod", "fsl", validValues=c("fsl","niftyreg","none"))
    
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
            session$unlinkDirectory("diffusion", ask=interactive)
            workingDir <- session$getDirectory("diffusion", createIfMissing=TRUE)
            
            if (is.null(dicomDir))
                dicomDir <- session$getDirectory()
            else if (!(dicomDir %~% "^([A-Za-z]:)?/"))
                dicomDir <- file.path(session$getDirectory(), dicomDir)
            dicomDir <- gsub("//+", "/", dicomDir, perl=TRUE)

            info <- readDicomDirectory(dicomDir, readDiffusionParams=TRUE)

            writeImageFile(info$image, session$getImageFileNameByType("rawdata","diffusion"))
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
                report(OL$Warning, "Diffusion direction information not available - you need to create a gradient table manually")
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
                report(OL$Info, "There are ", length(zeroes), " T2-weighted (b=", minBValue, ") volumes")
                choice <- -1

                while (!(choice %in% seq_along(zeroes)))
                {
                    choice <- ask("Use which one as the reference [1-", length(zeroes), "; s to show in fslview]?")
                    if (tolower(choice) == "s")
                    {
                        zeroVolumes <- readImageFile(session$getImageFileNameByType("rawdata","diffusion"), volumes=zeroes)
                        showImagesInViewer(zeroVolumes, viewer="fslview")
                    }
                    else
                        choice <- as.integer(choice)
                }
                
                choice <- zeroes[choice]
            }
            
            writeLines(as.character(choice), file.path(session$getDirectory("diffusion"),"refb0-index.txt"))
            
            report(OL$Info, "Extracting reference volume")
            refVolume <- readImageFile(session$getImageFileNameByType("rawdata","diffusion"), volumes=choice)
            writeImageFile(refVolume, session$getImageFileNameByType("refb0"))
        }
        
        if (runStages[3] && (!skipCompleted || !stagesComplete[3]))
        {
            done <- "n"
            repeat
            {
                if (tolower(done) != "s")
                {
                    if (maskingMethod == "bet")
                        runBetWithSession(session, betIntensityThreshold, betVerticalGradient)
                    else
                        createMaskImageForSession(session, maskingMethod, nClusters=nClusters)
                }
                
                if (interactive && maskingMethod != "fill")
                {
                    done <- ask("Is the brain extraction satisfactory? [yn; s to show the mask]")
                    
                    if (tolower(done) == "s")
                        showImagesInViewer(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("mask","diffusion"), lookupTable=c("greyscale","yellow"), opacity=c(1,0.5))
                    else if (tolower(done) == "y")
                        break
                    else if (maskingMethod == "bet")
                    {
                        report(OL$Info, "Previous intensity threshold was ", betIntensityThreshold, "; smaller values give larger brain outlines")
                        tempValue <- ask("Intensity threshold? [0 to 1; Enter for same as before]")
                        if (tempValue != "")
                            betIntensityThreshold <- as.numeric(tempValue)

                        report(OL$Info, "Previous vertical gradient was ", betVerticalGradient, "; positive values shift the outline downwards")
                        tempValue <- ask("Vertical gradient? [-1 to 1; Enter for same as before]")
                        if (tempValue != "")
                            betVerticalGradient <- as.numeric(tempValue)
                    }
                    else if (maskingMethod == "kmeans")
                    {
                        report(OL$Info, "Previous number of clusters was ", nClusters, "; larger values will usually give larger brain outlines")
                        tempValue <- ask("Number of clusters? [2 to 10; b to switch to using FSL-BET]")
                        if (tempValue == "b")
                            maskingMethod <- "bet"
                        else
                            nClusters <- as.integer(tempValue)
                    }
                }
                else
                    break
            }
        }
        
        if (runStages[4] && (!skipCompleted || !stagesComplete[4]))
        {
            refVolume <- as.integer(readLines(file.path(session$getDirectory("diffusion"),"refb0-index.txt")))
            if (eddyCorrectMethod == "fsl")
                runEddyCorrectWithSession(session, refVolume)
            else
            {
                scheme <- newSimpleDiffusionSchemeFromSession(session)
                bValues <- scheme$expandComponents()$bValues
                nLevels <- ifelse(bValues>1500, 3, 2)
                coregisterDataVolumesForSession(session, "diffusion", refVolume, useMask=FALSE, nLevels=nLevels, method=eddyCorrectMethod)
            }
        }
    }
    
    invisible (NULL)
}
