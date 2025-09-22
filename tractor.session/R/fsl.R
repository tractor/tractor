getFslVersion <- function ()
{
    fslHome <- Sys.getenv("FSLDIR")
    if (!is.character(fslHome) || nchar(fslHome) == 0)
        return (NULL)
    
    versionFile <- file.path(fslHome, "etc", "fslversion")
    if (!file.exists(versionFile))
        return (NULL)
    
    version <- as.integer(unlist(strsplit(readLines(versionFile)[1], ".", fixed=TRUE)))
    if (length(version) < 3)
        return (NULL)
    else
        return (sum(version[1:3] * c(10000, 100, 1)))
}

showImagesInFsleyes <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-cm", lookupTable, sep=" ")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-a", round(100*opacity), sep=" ")
    }
    
    execute("fsleyes", imageFileNames, errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}

showImagesInFslview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-l", lookupTable, sep=" ")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-t", opacity, sep=" ")
    }
    
    if (!is.null(locateExecutable("fslview_deprecated", errorIfMissing=FALSE)))
        execute("fslview_deprecated", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    else
        execute("fslview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}

createAcquisitionParameterFileForSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL, writeBZeroes = TRUE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!session$imageExists("rawdata","diffusion"))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    
    bValues <- session$getDiffusionScheme()$getBValues()
    bZeroVolumes <- which(bValues == min(bValues))
    nBZeroVolumes <- length(bZeroVolumes)
    bZeroData <- NULL
    
    if (writeBZeroes)
    {
        bZeroData <- session$getImageByType("rawdata", "diffusion", volumes=bZeroVolumes)
        writeImageFile(bZeroData, file.path(targetDir,"b0vols"))
    }
    
    phaseFile <- file.path(targetDir, "acqparams.txt")
    
    # Give the user a chance to override our guesswork, e.g. if their phase-encode direction is not A-P
    if (!file.exists(phaseFile))
    {
        if (is.null(reversePEVolumes))
        {
            report(OL$Info, "Reverse phase-encode volumes not specified - attempting to guess")
            if (is.null(bZeroData))
                bZeroData <- session$getImageByType("rawdata", "diffusion", volumes=bZeroVolumes)
            bZeroVolumeData <- lapply(seq_len(nBZeroVolumes), function(i) extractMriImage(bZeroData,4,i))
            
            similarities <- matrix(0, nBZeroVolumes, nBZeroVolumes)
            for (i in 1:nBZeroVolumes)
            {
                for (j in 1:i)
                    similarities[i,j] <- similarities[j,i] <- RNiftyReg::similarity(bZeroVolumeData[[i]], bZeroVolumeData[[j]])
            }
            distances <- outer(diag(similarities), diag(similarities), pmin) - similarities
            classes <- cutree(hclust(as.dist(distances)), 2)
            reversePEVolumes <- which(classes == which.min(table(classes)))
            report(OL$Info, "#{pluralise('Volume',reversePEVolumes)} #{implode(bZeroVolumes[reversePEVolumes],',',' and ',ranges=TRUE)} #{pluralise('is',reversePEVolumes,plural='are')} least similar to the other b=0 volumes")
        }
        else if (!all(reversePEVolumes %in% bZeroVolumes))
        {
            report(OL$Warning, "Not all reverse phase-encode volumes have b=0")
            return (NULL)
        }
        else
            reversePEVolumes <- match(reversePEVolumes, bZeroVolumes)
    
        # Assuming anterior-posterior phase-encoding direction here
        phaseEncoding <- t(c(0,1,0)) %x% matrix(1,nBZeroVolumes,1)
        phaseEncoding[reversePEVolumes,2] <- -1
    
        # Use the specified echo separation if available, otherwise use the file, or failing that just zero
        if (!is.null(echoSeparation))
            echoSeparation <- rep(echoSeparation, length.out=nBZeroVolumes)
        else
        {
            echoSeparationFile <- file.path(session$getDirectory("diffusion"), "echosep.txt")
            if (file.exists(echoSeparationFile))
            {
                echoSeparation <- as.numeric(readLines(echoSeparationFile))[bZeroVolumes]
                invalid <- (is.na(echoSeparation) | echoSeparation == 0)
                if (all(invalid))
                    echoSeparation <- rep(0.01, nBZeroVolumes)
                else if (all(echoSeparation[!invalid] == echoSeparation[!invalid][1]))
                    echoSeparation[invalid] <- echoSeparation[!invalid][1]
                else
                    echoSeparation[invalid] <- 0.01
            }
            else
                echoSeparation <- rep(0.01, nBZeroVolumes)
        }
    
        lines <- apply(cbind(phaseEncoding,echoSeparation), 1, implode, sep=" ")
        writeLines(lines, phaseFile)
    }
    
    return (phaseFile)
}

runTopupWithSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL)
{
    session <- as(session, "MriSession")
    session$getDirectory("fdt", createIfMissing=TRUE)
    createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation)
    report(OL$Info, "Running topup to correct susceptibility induced distortions...")
    runWorkflow("topup", session)
}

runEddyWithSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL)
{
    session <- as(session, "MriSession")
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation, writeBZeroes=FALSE)
    session$updateDiffusionScheme()
    
    bValues <- session$getDiffusionScheme()$getBValues()
    bZeroVolumes <- which(bValues == min(bValues))
    indices <- sapply(seq_along(bValues), function(i) {
        j <- i - bZeroVolumes
        which.min(replace(j, j<0, Inf))
    })
    
    indexFile <- file.path(targetDir, "index.txt")
    writeLines(implode(indices," "), indexFile)
    
    report(OL$Info, "Running eddy to remove eddy current induced artefacts...")
    runWorkflow("eddy", session)
    
    readEddyCorrectTransformsForSession(session)
}

runEddyCorrectWithSession <- function (session, refVolume)
{
    report(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    runWorkflow("eddycorrect", session, ReferenceVolume=refVolume)
    readEddyCorrectTransformsForSession(session)
}

readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    sourcePath <- session$getImageFileNameByType("rawdata", "diffusion")
    targetPath <- session$getImageFileNameByType("refb0", "diffusion")
    registration <- tractor.reg::createRegistration(sourcePath, targetPath, "fsl")
    
    eddyParamsFile <- file.path(session$getDirectory("fdt"), "data.eddy_parameters")
    eddyCorrectLogFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (file.exists(eddyParamsFile))
    {
        corrections <- as.matrix(read.table(eddyParamsFile))
        affines <- lapply(seq_len(nrow(corrections)), function(i) RNiftyReg::invertAffine(RNiftyReg::buildAffine(translation=corrections[i,1:3], angles=corrections[i,4:6], source=targetPath)))
    }
    else if (file.exists(eddyCorrectLogFile))
    {
        logLines <- readLines(eddyCorrectLogFile)
        logLines <- subset(logLines, logLines %~% "^[0-9\\-\\. ]+$")
        
        connection <- textConnection(logLines)
        matrices <- as.matrix(read.table(connection))
        close(connection)
        
        if (is.null(index))
            index <- seq_len(nrow(matrices) / 4)
        
        affines <- lapply(index, function(i) RNiftyReg:::convertAffine(matrices[(((i-1)*4)+1):(i*4),], sourcePath, targetPath))
    }
    else
        report(OL$Error, "No eddy current correction log was found")
    
    registration$setTransforms(affines, "affine")
    registration$serialise(file.path(session$getDirectory("diffusion"), "coreg_xfm.Rdata"))
    invisible(registration)
}

runDtifitWithSession <- function (session, weightedLeastSquares = FALSE)
{
    session$getDirectory("fdt", createIfMissing=TRUE)
    session$updateDiffusionScheme()
    runWorkflow("dtifit", session, WeightedLeastSquares=as.integer(weightedLeastSquares))
}

runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0)
{
    runWorkflow("bet-diffusion", session, IntensityThreshold=intensityThreshold, VerticalGradient=verticalGradient)
}

runBedpostWithSession <- function (session, nFibres = 3, how = c("fg","bg","screen"))
{
    session <- as(session, "MriSession")
    session$unlinkDirectory("bedpost")
    modelSpecification <- ifelse(session$getDiffusionScheme()$nShells() > 1, "\"-model 2\"", "")
    runWorkflow("bedpostx", session, Context=match.arg(how), FibresPerVoxel=nFibres, ModelSpec=modelSpecification)
}

getBedpostNumberOfFibresForSession <- function (session)
{
    return (getImageCountForSession(session, "avf", "bedpost"))
}
