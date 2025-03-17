#@args [session directory]
#@desc Run a standard preprocessing pipeline on the diffusion data in the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI/MGH volume; (2) optionally correct for susceptibility-induced distortions with FSL-TOPUP, then identify a volume with little or no diffusion weighting to use as an anatomical reference; (3) create a mask to identify voxels which are within the brain; (4) correct the data set for eddy current induced distortions. Stage 4 is currently performed using FSL "eddy" by default, but the older (and quicker) "eddy_correct", or TractoR's native NiftyReg back-end, can be used instead. Stage 3 will use FSL's brain extraction tool for accuracy if it is available, otherwise it will fall back to a k-means approach, which may also be more successful for nonbrain data. (The KMeansClusters option can be increased if the mask does not cover the whole region of interest.) Some reversed phase-encode acquisitions are suggested for TOPUP and eddy, but this wrapper currently only supports phase-encode reversed volumes with b=0. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying Force:true.) Giving StatusOnly:true will report which stages have been run. The script asks the user about each stage unless Interactive:false is given.
#@example # The standard pipeline in TractoR 2.x
#@example tractor dpreproc UseTopup:false MaskingMethod:kmeans EddyCorrectionMethod:eddycorrect
#@group Diffusion processing
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    sessionPath <- ifelse(nArguments()==0, ".", Arguments[1])
    if (!file.exists(sessionPath))
        dir.create(sessionPath)
    session <- attachMriSession(sessionPath)
    
    statusOnly <- getConfigVariable("StatusOnly", FALSE)
    interactive <- getConfigVariable("Interactive", TRUE)
    stages <- getConfigVariable("RunStages", "1-4", "integer", multiple=TRUE)
    force <- getConfigVariable("Force", FALSE)
    dicomDirs <- getConfigVariable("DicomDirectories", NULL, "character")
    dicomReader <- getConfigVariable("DicomReader", "internal", "character", validValues=c("internal","divest"))
    useGradientCache <- getConfigVariable("UseGradientCache", "second", validValues=c("first","second","never"))
    flipAxes <- getConfigVariable("FlipGradientAxes", NULL, "character")
    useTopup <- getConfigVariable("UseTopup", TRUE)
    reversePEVolumes <- getConfigVariable("ReversePEVolumes", "none")
    echoSeparation <- getConfigVariable("EchoSeparation", NULL, "numeric")
    maskingMethod <- getConfigVariable("MaskingMethod", "bet", validValues=c("bet","kmeans","fill"))
    nClusters <- getConfigVariable("KMeansClusters", 2, "integer")
    betIntensityThreshold <- getConfigVariable("BetIntensityThreshold", 0.3)
    betVerticalGradient <- getConfigVariable("BetVerticalGradient", 0)
    eddyCorrectMethod <- getConfigVariable("EddyCorrectionMethod", "eddy", validValues=c("eddy","eddycorrect","fsl","niftyreg","none"))
    
    if ((interactive || statusOnly) && getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    if (!is.null(flipAxes))
    {
        if (flipAxes %~% "\\d")
            report(OL$Warning, "The \"FlipGradientAxes\" option should be specified as a comma-separated list of axis labels, as in \"x,y\"")
        flipAxes <- which(letters[24:26] %in% splitAndConvertString(flipAxes, ",", fixed=TRUE))
    }
    
    if (reversePEVolumes == "none")
        reversePEVolumes <- integer(0)
    else if (reversePEVolumes == "auto")
        reversePEVolumes <- NULL
    else
        reversePEVolumes <- splitAndConvertString(reversePEVolumes, ",", "integer", fixed=TRUE)
    
    runStages <- 1:4 %in% stages
    if (all(!runStages))
        report(OL$Info, "Nothing to do")
    
    stagesComplete <- c(session$imageExists("rawdata","diffusion"),
                        session$imageExists("refb0","diffusion"),
                        session$imageExists("mask","diffusion"),
                        session$imageExists("data","diffusion"))

    if (statusOnly)
        printLabelledValues(c("Session directory","Stages completed"), c(session$getDirectory(),implode(which(stagesComplete),", ")))
    else
    {
        if (runStages[1] && (force || !stagesComplete[1]))
        {
            echoSeparations <- seriesDescriptions <- NULL
            
            # Reconstruct a 4D "rawdata" image from already-converted subsets, or directly from DICOM
            rawDataPath <- session$getImageFileNameByType("rawdata", "diffusion")
            if (imageFileExists(es("#{rawDataPath}_1")))
            {
                pattern <- ore(ore.escape(basename(rawDataPath)), "_(\\d+)\\.")
                n <- max(as.integer(groups(ore.search(pattern, list.files(dirname(rawDataPath))))))
                fileNames <- sapply(seq_len(n), function(i) es("#{rawDataPath}_#{i}"))
                
                report(OL$Info, "Merging #{n} raw data series")
                images <- lapply(fileNames, readImageFile)
                mergedImage <- do.call(mergeMriImages, c(images, list(bindDim=4L, padTags=TRUE)))
                seriesDescriptions <- unique(mergedImage$getTags("seriesDescription"))
                if (mergedImage$hasTags("effectiveReadoutTime"))
                    echoSeparations <- mergedImage$getTags("effectiveReadoutTime")
                else if (all(mergedImage$hasTags(c("echoSpacing", "epiFactor"))))
                    echoSeparations <- mergedImage$getTags("echoSpacing") / 1e6 * (mergedImage$getTags("epiFactor") - 1)
            }
            else
            {
                session$unlinkDirectory("diffusion", ask=interactive)
                session$getDirectory("diffusion", createIfMissing=TRUE)
                
                dicomDirsSpecified <- !is.null(dicomDirs)
                if (!dicomDirsSpecified)
                    dicomDirs <- session$getDirectory()
                
                # Non-absolute paths are relative to the session directory
                dicomDirs <- splitAndConvertString(dicomDirs, ",", fixed=TRUE)
                dicomDirs <- ifelse(dicomDirs %~% "^([A-Za-z]:)?/", dicomDirs, file.path(session$getDirectory(),dicomDirs))
                dicomDirs <- ore.subst("//+", "/", dicomDirs, all=TRUE)
                
                info <- NULL
                if (dicomReader == "internal")
                    info <- lapply(dicomDirs, readDicomDirectory, method="internal", readDiffusionParams=TRUE)
                else if (interactive || dicomDirsSpecified)
                    info <- lapply(dicomDirs, readDicomDirectory, method="divest", readDiffusionParams=TRUE, interactive=interactive)
                else
                    info <- lapply(dicomDirs, readDicomDirectory, method="divest", readDiffusionParams=TRUE, interactive=FALSE, subset=diffusion)
                
                mergedImage <- do.call(mergeMriImages, c(lapply(info, "[[", "image"), list(bindDim=4L, padTags=TRUE)))
                seriesDescriptions <- do.call(c, lapply(info, "[[", "seriesDescriptions"))
                echoSeparations <- do.call(c, lapply(info, "[[", "echoSeparations"))
            }
            
            writeImageFile(mergedImage, session$getImageFileNameByType("rawdata","diffusion"), writeTags=TRUE)
            print(mergedImage)
            
            if (!is.null(seriesDescriptions))
            {
                seriesDescriptions <- implode(ore.subst("\\W","_",seriesDescriptions,all=TRUE), ",")
                writeLines(seriesDescriptions, file.path(session$getDirectory("diffusion"),"descriptions.txt"))
            }
            
            if (all(mergedImage$hasTags(c("bValues", "bVectors"))))
            {
                bValues <- mergedImage$getTags("bValues")
                bVectors <- mergedImage$getTags("bVectors")
                missing <- (is.na(bValues) | apply(is.na(bVectors),1,any))
                if (any(missing))
                {
                    report(OL$Warning, "Gradient information for #{pluralise('volume',n=sum(missing))} #{implode(which(missing),',',' and ',ranges=TRUE)} is missing - treating as zero")
                    bValues[missing] <- 0
                    bVectors[missing,] <- 0
                }
                report(OL$Info, "Constructing acquisition scheme")
                scheme <- asDiffusionScheme(bVectors, bValues)
                print(scheme)
            }
            
            if (!is.null(echoSeparations) && any(!is.na(echoSeparations)))
                writeLines(as.character(echoSeparations), file.path(session$getDirectory("diffusion"),"echosep.txt"))
            
            if (useGradientCache == "first" || (useGradientCache == "second" && is.null(session$getDiffusionScheme())))
            {
                gradientSet <- checkGradientCacheForSession(session)
                if (!is.null(gradientSet))
                {
                    report(OL$Info, "Gradient cache hit - using stored gradient scheme")
                    scheme <- asDiffusionScheme(gradientSet[,1:3,drop=FALSE], gradientSet[,4])
                    session$updateDiffusionScheme(scheme, unrotated=TRUE)
                }
            }
            
            if (is.null(session$getDiffusionScheme()))
                report(OL$Warning, "Diffusion direction information not available - you need to create a gradient table manually")
            else
            {
                if (!is.null(flipAxes) && length(flipAxes) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
                if (isTRUE(updateGradientCacheFromSession(session)))
                    report(OL$Info, "Gradient directions inserted into cache for future reference")
            }
        }
    
        if (runStages[2] && (force || !stagesComplete[2]))
        {
            scheme <- session$getDiffusionScheme()
            if (is.null(scheme))
                report(OL$Error, "No b-value or gradient direction information is available")
            
            b0Path <- session$getImageFileNameByType("rawdata", "diffusion")
            if (useTopup && eddyCorrectMethod == "eddy")
            {
                if (!precheckWorkflow("topup", commandOnly=TRUE)$ok)
                {
                    report(OL$Warning, "The \"topup\" executable was not found - continuing without it")
                    useTopup <- FALSE
                }
                else
                {
                    runTopupWithSession(session, reversePEVolumes, echoSeparation)
                    b0Path <- file.path(session$getDirectory("fdt"), "b0corrected")
                }
            }
            
            bValues <- scheme$getBValues()
            minBValue <- min(bValues)
            zeroes <- which(bValues == minBValue)
            if (length(zeroes) == 1)
            {
                choice <- 1
                report(OL$Info, "Volume #{choice} is the only T2-weighted (b=#{minBValue}) volume in the data set")
            }
            else if (!interactive)
            {
                choice <- 1
                report(OL$Info, "Using volume #{choice} with b=#{minBValue} as the reference volume")
            }
            else
            {
                report(OL$Info, "There #{pluralise('is',zeroes,plural='are')} #{length(zeroes)} T2-weighted (b=#{minBValue}) #{pluralise('volume',zeroes)}")
                
                repeat
                {
                    choice <- ask("Use which one as the reference [1-#{length(zeroes)}; s to show in fslview]?", valid=c("s",seq_along(zeroes)))
                    if (choice == "s")
                    {
                        zeroVolumes <- readImageFile(b0Path, volumes=(if(useTopup) NULL else zeroes))
                        showImagesInViewer(zeroVolumes, viewer="fslview")
                    }
                    else
                    {
                        choice <- as.integer(choice)
                        break
                    }
                }
            }
        
            writeLines(as.character(zeroes[choice]), file.path(session$getDirectory("diffusion"),"refb0-index.txt"))
        
            report(OL$Info, "Extracting reference volume")
            refVolume <- readImageFile(b0Path, volumes=(if(useTopup) choice else zeroes[choice]))
            writeImageFile(refVolume, session$getImageFileNameByType("refb0"))
        }
        
        if (runStages[3] && (force || !stagesComplete[3]))
        {
            if (maskingMethod == "bet" && !precheckWorkflow("bet-diffusion",commandOnly=TRUE)$ok)
            {
                report(OL$Warning, "The \"bet\" executable was not found - using k-means instead")
                maskingMethod <- "kmeans"
            }
            
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
                
                if (!interactive || maskingMethod == "fill")
                    break
                
                done <- ask("Is the brain extraction satisfactory? [yn; s to show the mask]", valid=c("y","n","s"))
                
                if (done == "y")
                    break
                else if (done == "s")
                    showImagesInViewer(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("mask","diffusion"), lookupTable=c("greyscale","yellow"), opacity=c(1,0.5))
                else if (maskingMethod == "bet")
                {
                    report(OL$Info, "Previous intensity threshold was #{betIntensityThreshold}; smaller values give larger brain outlines")
                    tempValue <- ask("Intensity threshold? [0 to 1; Enter for same as before]")
                    if (tempValue != "")
                        betIntensityThreshold <- as.numeric(tempValue)

                    report(OL$Info, "Previous vertical gradient was #{betVerticalGradient}; positive values shift the outline downwards")
                    tempValue <- ask("Vertical gradient? [-1 to 1; Enter for same as before]")
                    if (tempValue != "")
                        betVerticalGradient <- as.numeric(tempValue)
                }
                else if (maskingMethod == "kmeans")
                {
                    report(OL$Info, "Previous number of clusters was #{nClusters}; larger values will usually give larger brain outlines")
                    tempValue <- ask("Number of clusters? [2 to 10; b to switch to using FSL-BET]", valid=c("b",2:10))
                    if (tempValue == "b")
                        maskingMethod <- "bet"
                    else
                        nClusters <- as.integer(tempValue)
                }
            }
        }
        
        if (runStages[4] && (force || !stagesComplete[4]))
        {
            refVolume <- as.integer(readLines(file.path(session$getDirectory("diffusion"),"refb0-index.txt")))
            
            if (eddyCorrectMethod == "eddy" && precheckWorkflow("eddy",commandOnly=TRUE)$ok)
                runEddyWithSession(session, reversePEVolumes, echoSeparation)
            else if (eddyCorrectMethod %in% c("eddycorrect","fsl") && precheckWorkflow("eddycorrect",commandOnly=TRUE)$ok)
                runEddyCorrectWithSession(session, refVolume)
            else
            {
                if (eddyCorrectMethod != "none")
                    eddyCorrectMethod <- "niftyreg"
                coregisterDataVolumesForSession(session, "diffusion", refVolume, useMask=FALSE, nLevels=2, method=eddyCorrectMethod, options=list(symmetric=FALSE))
            }
        }
    }
    
    invisible (NULL)
}
