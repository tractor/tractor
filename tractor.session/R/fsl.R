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
        report(OL$Error, "The specified session does not contain a \"basic\" 4D image")
    if (!all(file.exists(file.path(session$getDirectory("fdt"), c("bvals","bvecs")))))
        report(OL$Error, "The specified session does not contain \"bvals\" and \"bvecs\" files")
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
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
}

runDtifitWithSession <- function (session, showOnly = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getDirectory("fdt")
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")) || !imageFileExists(session$getImageFileNameByType("data","diffusion")) || !imageFileExists(session$getImageFileNameByType("mask","diffusion")))
        report(OL$Error, "Some required files are missing - run eddy_correct and bet first")
    
    if (showOnly)
    {
        if (!imageFileExists(session$getImageFileNameByType("fa")))
            report(OL$Warning, "Cannot display tensor directions because dtifit has not yet been run")
        else
        {
            paramString <- paste(session$getImageFileNameByType("fa"), session$getImageFileNameByType("eigenvector","diffusion",index=1), sep=" ")
            execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
        }
    }
    else
    {
        report(OL$Info, "Running dtifit to do tensor fitting...")
        paramString <- paste("-k", session$getImageFileNameByType("data","diffusion"), "-m", session$getImageFileNameByType("mask","diffusion"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(session$getDirectory("diffusion"),"dti"), sep=" ")
        execute("dtifit", paramString, errorOnFail=TRUE)
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
        paramString <- paste(session$getImageFileNameByType("refb0"), session$getImageFileNameByType("maskedb0"), "-m -v -f", intensityThreshold, "-g", verticalGradient, sep=" ")
        execute("bet", paramString, errorOnFail=TRUE)
    }
}

runBedpostWithSession <- function (session, nFibres = 2, how = c("fg","bg","screen"), ask = TRUE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    how <- match.arg(how)
    
    targetDir <- session$getDirectory("fdt")
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")) || !imageFileExists(session$getImageFileNameByType("data","diffusion")) || !imageFileExists(session$getImageFileNameByType("mask","diffusion")))
        report(OL$Error, "Some required files are missing - run eddy_correct and bet first")
    
    if (!ask)
        ans <- "y"
    else
    {
        execute("bedpostx_datacheck", session$getDirectory("diffusion"), errorOnFail=TRUE)
        ans <- report(OL$Question, "Okay to start bedpostx [yn]?")
    }
    
    if (tolower(ans) == "y")
    {
        unlink(session$getDirectory("bedpost"), recursive=TRUE)
        
        if (how == "screen")
        {
            report(OL$Info, "Starting bedpostx in a \"screen\" session")
            bedpostLoc <- locateExecutable("bedpostx", errorIfMissing=TRUE)
            paramString <- paste("-d -m", bedpostLoc, session$getDirectory("diffusion"), "-n", nFibres, sep=" ")
            execute("screen", paramString, errorOnFail=TRUE)
        }
        else
        {
            report(OL$Info, "Starting bedpostx as a ", ifelse(how=="fg","normal foreground","background"), " process")
            paramString <- paste(session$getDirectory("diffusion"), "-n", nFibres, sep=" ")
            execute("bedpostx", paramString, errorOnFail=TRUE, wait=(how=="fg"))
        }
    }
}
