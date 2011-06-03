getFslVersion <- function ()
{
    fslHome <- Sys.getenv("FSLDIR")
    if (!is.character(fslHome) || nchar(fslHome) == 0)
        return (NULL)
    
    versionFile <- file.path(fslHome, "etc", "fslversion")
    if (!file.exists(versionFile))
        return (NULL)
    
    version <- as.integer(unlist(strsplit(readLines(versionFile)[1], ".", fixed=TRUE)))
    if (length(version) != 3)
        return (NULL)
    else
        return (sum(version * c(10000, 100, 1)))
}

runEddyCorrectWithSession <- function (session, ask = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!imageFileExists(session$getImageFileNameByType("rawdata","diffusion")))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    if (is.null(scheme))
        report(OL$Error, "The specified session does not contain gradient direction information")
    
    schemeComponents <- scheme$expandComponents()
    minBValue <- min(scheme$getBValues())
    if (minBValue != 0)
        report(OL$Info, "Minimal b-value in this data set is ", minBValue, " rather than 0")
    
    zeroes <- which(schemeComponents$bValues == minBValue)
    if (length(zeroes) == 0)
        report(OL$Error, "No b-values are specified for this data set")
    else if (length(zeroes) == 1)
    {
        choice <- zeroes
        report(OL$Info, "Volume ", choice, " is the only T2 weighted volume in the data set")
    }
    else if (!ask)
    {
        choice <- zeroes[1]
        report(OL$Info, "Using volume ", choice, " as the reference volume")
    }
    else
    {
        report(OL$Info, "Volumes ", implode(zeroes,sep=", ",finalSep=" and "), " are T2 weighted")
        choice <- -1
        
        while (!(choice %in% zeroes))
        {
            choice <- report(OL$Question, "Use which one as the reference [s to show in fslview]?")
            if (tolower(choice) == "s")
            {
                # fslview is fussy about data types, so write the image into
                # Analyze format to avoid a crash
                image <- session$getImageByType("rawdata", "diffusion")
                imageLoc <- tempfile()
                writeMriImageToFile(image, imageLoc, fileType="ANALYZE_GZ")
                execute("fslview", imageLoc, errorOnFail=TRUE, wait=FALSE)
            }
            else
                choice <- as.numeric(choice)
        }
    }
    
    report(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    paramString <- paste(session$getImageFileNameByType("rawdata","diffusion"), session$getImageFileNameByType("data","diffusion"), choice-1, sep=" ")
    execute("eddy_correct", paramString, errorOnFail=TRUE)
    
    data <- session$getImageByType("data","diffusion")
    refVolume <- newMriImageByExtraction(data, 4, choice)
    writeMriImageToFile(refVolume, session$getImageFileNameByType("refb0"))
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    file.rename(file.path(session$getDirectory("diffusion"),"data.ecclog"), file.path(targetDir,"data.ecclog"))
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

runDtifitWithSession <- function (session, showOnly = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    
    if (showOnly)
    {
        if (!imageFileExists(session$getImageFileNameByType("fa","diffusion")))
            report(OL$Warning, "Cannot display tensor directions because dtifit has not yet been run")
        else
        {
            paramString <- paste(session$getImageFileNameByType("fa","diffusion"), session$getImageFileNameByType("eigenvector","diffusion",index=1), sep=" ")
            execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
        }
    }
    else
    {
        report(OL$Info, "Running dtifit to do tensor fitting...")
        paramString <- paste("-k", session$getImageFileNameByType("data","fdt"), "-m", session$getImageFileNameByType("mask","fdt"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(targetDir,"dti"), sep=" ")
        execute("dtifit", paramString, errorOnFail=TRUE)
        
        names <- c("s0", "fa", "md", "eigenvalue", "eigenvalue", "eigenvalue", "eigenvector", "eigenvector", "eigenvector")
        indices <- c(1, 1, 1, 1:3, 1:3)
        for (i in seq_along(names))
            symlinkImageFiles(session$getImageFileNameByType(names[i],"fdt",indices[i]), session$getImageFileNameByType(names[i],"diffusion",indices[i]), overwrite=TRUE)
    }
}

runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0, showOnly = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    if (!imageFileExists(session$getImageFileNameByType("refb0")))
        report(OL$Error, "A reference b=0 image file has not yet been corrected - run eddy_correct first")
    
    if (showOnly)
    {
        paramString <- paste(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("mask","diffusion"), sep=" ")
        execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
    }
    else
    {
        report(OL$Info, "Running FSL's brain extraction tool...")
        paramString <- paste(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("maskedb0"), "-m -f", intensityThreshold, "-g", verticalGradient, sep=" ")
        execute("bet", paramString, errorOnFail=TRUE)
        
        copyImageFiles(paste(session$getImageFileNameByType("maskedb0"),"mask",sep="_"), session$getImageFileNameByType("mask","diffusion"), overwrite=TRUE, deleteOriginals=TRUE)
    }
}

runBedpostWithSession <- function (session, nFibres = 2, how = c("fg","bg","screen"))
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    how <- match.arg(how)
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createFdtFilesForSession(session)
    
    unlink(session$getDirectory("bedpost"), recursive=TRUE)
    
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
