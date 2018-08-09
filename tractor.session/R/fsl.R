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
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!session$imageExists("rawdata","diffusion"))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    phaseFile <- createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation)
    
    report(OL$Info, "Running topup to correct susceptibility induced distortions...")
    params <- es(c("--imain=#{file.path(targetDir,'b0vols')}", "--datain=#{phaseFile}", "--config=b02b0.cnf", "--out=#{file.path(targetDir,'topup')}", "--iout=#{file.path(targetDir,'b0corrected')}"))
    execute("topup", params, errorOnFail=TRUE)
}

runEddyWithSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!session$imageExists("rawdata","diffusion"))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    phaseFile <- createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation, writeBZeroes=FALSE)
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
    params <- es(c("--imain=#{session$getImageFileNameByType('rawdata','diffusion')}", "--mask=#{session$getImageFileNameByType('mask','diffusion')}", "--acqp=#{phaseFile}", "--index=#{indexFile}", "--bvecs=#{file.path(targetDir,'bvecs')}", "--bvals=#{file.path(targetDir,'bvals')}", "--out=#{file.path(targetDir,'data')}"))
    if (imageFileExists(file.path(targetDir, "topup_fieldcoef")))
        params <- c(params, es("--topup=#{file.path(targetDir,'topup')}"))
    execute("eddy", params, errorOnFail=TRUE, stdout=file.path(targetDir,"eddy.log"))
    
    mainDataPath <- session$getImageFileNameByType("data", "diffusion")
    fdtDataPath <- session$getImageFileNameByType("data", "fdt")
    copyImageFiles(fdtDataPath, mainDataPath, overwrite=TRUE, deleteOriginals=TRUE)
    symlinkImageFiles(mainDataPath, fdtDataPath)
    
    readEddyCorrectTransformsForSession(session)
}

runEddyCorrectWithSession <- function (session, refVolume)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!session$imageExists("rawdata","diffusion"))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    report(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    paramString <- paste(session$getImageFileNameByType("rawdata","diffusion"), session$getImageFileNameByType("data","diffusion"), refVolume-1, sep=" ")
    execute("eddy_correct", paramString, errorOnFail=TRUE)
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    file.rename(file.path(session$getDirectory("diffusion"),"data.ecclog"), file.path(targetDir,"data.ecclog"))
    
    readEddyCorrectTransformsForSession(session)
}

readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    sourcePath <- session$getImageFileNameByType("rawdata", "diffusion")
    targetPath <- session$getImageFileNameByType("refb0", "diffusion")
    transform <- tractor.reg::Transformation$new(threadSafeTempFile(), sourcePath, targetPath)
    
    eddyParamsFile <- file.path(session$getDirectory("fdt"), "data.eddy_parameters")
    eddyCorrectLogFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (file.exists(eddyParamsFile))
    {
        corrections <- as.matrix(read.table(eddyParamsFile))
        affines <- lapply(seq_len(nrow(corrections)), function(i) solve(RNiftyReg::buildAffine(translation=corrections[i,1:3], angles=corrections[i,4:6], source=targetPath)))
        transform$updateFromObjects(affineMatrices=affines, method="fsl")
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
        
        matrices <- lapply(index, function(i) matrices[(((i-1)*4)+1):(i*4),])
        transform$updateFromObjects(affineMatrices=matrices, method="fsl", convert=TRUE)
    }
    else
        report(OL$Error, "No eddy current correction log was found")
    
    transform$move(file.path(session$getDirectory("diffusion"), "coreg"))
    invisible (transform)
}

createFdtFilesForSession <- function (session, overwriteExisting = FALSE)
{
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    dataImageName <- session$getImageFileNameByType("data", "fdt")
    maskImageName <- session$getImageFileNameByType("mask", "fdt")
    
    if (overwriteExisting || !file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")))
        session$updateDiffusionScheme()
    if (overwriteExisting || !imageFileExists(dataImageName))
    {
        targetDataImageName <- session$getImageFileNameByType("data", "diffusion")
        if (!imageFileExists(targetDataImageName))
            report(OL$Error, "Data image has not been created yet")
        
        symlinkImageFiles(targetDataImageName, dataImageName, overwrite=TRUE)
    }
    if (overwriteExisting || !imageFileExists(maskImageName))
    {
        targetMaskImageName <- session$getImageFileNameByType("mask", "diffusion")
        if (!imageFileExists(targetMaskImageName))
            report(OL$Error, "Mask image has not been created yet")
        
        symlinkImageFiles(targetMaskImageName, maskImageName, overwrite=TRUE)
    }
}

runDtifitWithSession <- function (session, weightedLeastSquares = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    
    report(OL$Info, "Running dtifit to do tensor fitting...")
    paramString <- paste("-k", session$getImageFileNameByType("data","fdt"), "-m", session$getImageFileNameByType("mask","fdt"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(targetDir,"dti"), "--sse", ifelse(weightedLeastSquares,"-w",""), sep=" ")
    execute("dtifit", paramString, errorOnFail=TRUE)
    
    names <- c("s0", "fa", "md", "eigenvalue", "eigenvalue", "eigenvalue", "eigenvector", "eigenvector", "eigenvector", "sse")
    indices <- c(1, 1, 1, 1:3, 1:3, 1)
    for (i in seq_along(names))
        symlinkImageFiles(session$getImageFileNameByType(names[i],"fdt",indices[i]), session$getImageFileNameByType(names[i],"diffusion",indices[i]), overwrite=TRUE)
}

runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    if (!imageFileExists(session$getImageFileNameByType("refb0")))
        report(OL$Error, "A reference b=0 image file has not yet been created - run eddy_correct first")
    
    report(OL$Info, "Running FSL's brain extraction tool...")
    paramString <- paste(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("maskedb0"), "-m -f", intensityThreshold, "-g", verticalGradient, sep=" ")
    execute("bet", paramString, errorOnFail=TRUE)
    
    copyImageFiles(paste(session$getImageFileNameByType("maskedb0"),"mask",sep="_"), session$getImageFileNameByType("mask","diffusion"), overwrite=TRUE, deleteOriginals=TRUE)
}

runBedpostWithSession <- function (session, nFibres = 3, how = c("fg","bg","screen"))
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    how <- match.arg(how)
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    session$unlinkDirectory("bedpost")
    
    modelArg <- ifelse(session$getDiffusionScheme()$nShells() > 1, "-model 2", "")
    
    if (how == "screen")
    {
        report(OL$Info, "Starting bedpostx in a \"screen\" session")
        bedpostLoc <- locateExecutable("bedpostx", errorIfMissing=TRUE)
        paramString <- paste("-d -m", bedpostLoc, targetDir, "-n", nFibres, modelArg, sep=" ")
        execute("screen", paramString, errorOnFail=TRUE)
    }
    else
    {
        report(OL$Info, "Starting bedpostx as a ", ifelse(how=="fg","normal foreground","background"), " process")
        paramString <- paste(targetDir, "-n", nFibres, modelArg, sep=" ")
        execute("bedpostx", paramString, errorOnFail=TRUE, wait=(how=="fg"))
    }
}

getBedpostNumberOfFibresForSession <- function (session)
{
    return (getImageCountForSession(session, "avf", "bedpost"))
}
