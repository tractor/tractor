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
    
    execute("fslview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}
runEddyWithSession <- function (session, useTopup = FALSE)
{
  if (!is(session, "MriSession"))
    report(OL$Error, "Specified session is not an MriSession object")
  if (!imageFileExists(session$getImageFileNameByType("rawdata","diffusion")))
    report(OL$Error, "The specified session does not contain a raw data image")

  # eddy --imain=rawdata --mask=brain_mask --acqp=acquisition --index=index --bvecs=bvecs --bvals=bvals --topup=topup --out=data
  report(OL$Info, "Running eddy to remove eddy current induced artefacts...")
  paramString <- paste(paste("--imain",session$getImageFileNameByType("rawdata","diffusion"), sep="="), paste("--out",session$getImageFileNameByType("data","diffusion"),sep="="), paste("--mask",session$getImageFileNameByType("mask","diffusion"),sep="="), sep=" ")
  
  if (useTopup && fileExists(session$getFileNameByType("topup","diffusion")))
    paramString <- paste(paramString, paste("--topup",session$getFileNameByType("topup","diffusion"),sep="="), sep=" ")
  
  execute("eddy", paramString, errorOnFail=TRUE)
  
  targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
  file.rename(file.path(session$getDirectory("diffusion"),"data.ecclog"), file.path(targetDir,"data.ecclog"))

  # commenting out for now...
  # transform <- readEddyCorrectTransformsForSession(session)
  # transform$serialise(file.path(session$getDirectory("diffusion"), "coreg_xfm.Rdata"))
}

runEddyCorrectWithSession <- function (session, refVolume)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!imageFileExists(session$getImageFileNameByType("rawdata","diffusion")))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    report(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    paramString <- paste(session$getImageFileNameByType("rawdata","diffusion"), session$getImageFileNameByType("data","diffusion"), refVolume-1, sep=" ")
    execute("eddy_correct", paramString, errorOnFail=TRUE)
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    file.rename(file.path(session$getDirectory("diffusion"),"data.ecclog"), file.path(targetDir,"data.ecclog"))
    
    transform <- readEddyCorrectTransformsForSession(session)
    transform$serialise(file.path(session$getDirectory("diffusion"), "coreg_xfm.Rdata"))
}

readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    logFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (!file.exists(logFile))
        report(OL$Error, "Eddy current correction log not found")
    logLines <- readLines(logFile)
    logLines <- subset(logLines, logLines %~% "^[0-9\\-\\. ]+$")
    
    connection <- textConnection(logLines)
    matrices <- as.matrix(read.table(connection))
    close(connection)
    
    if (is.null(index))
        index <- seq_len(nrow(matrices) / 4)
    
    matrices <- lapply(index, function(i) matrices[(((i-1)*4)+1):(i*4),])
    
    sourceImage <- session$getImageByType("rawdata", "diffusion", metadataOnly=TRUE)
    targetImage <- session$getImageByType("refb0", "diffusion", metadataOnly=TRUE)
    transform <- tractor.reg::Transformation$new(sourceImage=sourceImage, targetImage=targetImage, affineMatrices=matrices, controlPointImages=list(), reverseControlPointImages=list(), method="fsl", version=1)
    
    invisible (transform)
}

createFdtFilesForSession <- function (session, overwriteExisting = FALSE)
{
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    dataImageName <- session$getImageFileNameByType("data", "fdt")
    maskImageName <- session$getImageFileNameByType("mask", "fdt")
    
    if (overwriteExisting || !file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")))
    {
        scheme <- newSimpleDiffusionSchemeFromSession(session)
        if (is.null(scheme))
            report(OL$Error, "The specified session does not contain gradient direction information")
        
        writeSimpleDiffusionSchemeForSession(session, scheme, thirdPartyOnly=TRUE)
    }
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

runDtifitWithSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    
    report(OL$Info, "Running dtifit to do tensor fitting...")
    paramString <- paste("-k", session$getImageFileNameByType("data","fdt"), "-m", session$getImageFileNameByType("mask","fdt"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(targetDir,"dti"), "--sse", sep=" ")
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

runBedpostWithSession <- function (session, nFibres = 2, how = c("fg","bg","screen"))
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    how <- match.arg(how)
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    
    session$unlinkDirectory("bedpost")
    
    if (how == "screen")
    {
        report(OL$Info, "Starting bedpostx in a \"screen\" session")
        bedpostLoc <- locateExecutable("bedpostx", errorIfMissing=TRUE)
        paramString <- paste("-d -m", bedpostLoc, targetDir, "-n", nFibres, sep=" ")
        execute("screen", paramString, errorOnFail=TRUE)
    }
    else
    {
        report(OL$Info, "Starting bedpostx as a ", ifelse(how=="fg","normal foreground","background"), " process")
        paramString <- paste(targetDir, "-n", nFibres, sep=" ")
        execute("bedpostx", paramString, errorOnFail=TRUE, wait=(how=="fg"))
    }
}

getBedpostNumberOfFibresForSession <- function (session)
{
    return (getImageCountForSession(session, "avf", "bedpost"))
}
