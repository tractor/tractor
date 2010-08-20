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
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!imageFileExists(file.path(targetDir,"basic")))
        output(OL$Error, "The specified session does not contain a \"basic\" 4D image")
    if (!all(file.exists(file.path(targetDir, c("bvals","bvecs")))))
        output(OL$Error, "The specified session does not contain \"bvals\" and \"bvecs\" files")
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    schemeComponents <- scheme$expandComponents()
    minBValue <- min(scheme$getBValues())
    if (minBValue != 0)
        output(OL$Info, "Minimal b-value in this data set is ", minBValue, " rather than 0")
    
    zeroes <- which(schemeComponents$bValues == minBValue)
    if (length(zeroes) == 0)
        output(OL$Error, "No b-values are specified for this data set")
    else if (length(zeroes) == 1)
    {
        choice <- zeroes
        output(OL$Info, "Volume ", choice, " is the only T2 weighted volume in the data set")
    }
    else if (!ask)
    {
        choice <- zeroes[1]
        output(OL$Info, "Using volume ", choice, " as the reference volume")
    }
    else
    {
        output(OL$Info, "Volumes ", implode(zeroes,sep=", ",finalSep=" and "), " are T2 weighted")
        choice <- -1
        
        while (!(choice %in% zeroes))
        {
            choice <- output(OL$Question, "Use which one as the reference [s to show in fslview]?")
            if (tolower(choice) == "s")
            {
                # fslview is fussy about data types, so write the image into
                # Analyze format to avoid a crash
                image <- newMriImageFromFile(file.path(targetDir, "basic"))
                imageLoc <- tempfile()
                writeMriImageToFile(image, imageLoc, fileType="ANALYZE_GZ")
                execute("fslview", imageLoc, errorOnFail=TRUE, wait=FALSE)
            }
            else
                choice <- as.numeric(choice)
        }
    }
    
    output(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    paramString <- paste(file.path(targetDir,"basic"), file.path(targetDir,"data"), choice-1, sep=" ")
    execute("eddy_correct", paramString, errorOnFail=TRUE)
    
    data <- newMriImageFromFile(file.path(targetDir, "data"))
    refVolume <- newMriImageByExtraction(data, 4, choice)
    writeMriImageToFile(refVolume, file.path(targetDir,"nodif"))
}

runDtifitWithSession <- function (session, showOnly = FALSE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")) || !imageFileExists(file.path(targetDir,"data")) || !imageFileExists(file.path(targetDir,"nodif_brain_mask")))
        output(OL$Error, "Some required files are missing - run eddy_correct and bet first")
    
    if (showOnly)
    {
        if (!imageFileExists(file.path(targetDir, "dti_FA")))
            output(OL$Warning, "Cannot display tensor directions because dtifit has not yet been run")
        else
        {
            paramString <- paste(file.path(targetDir,"dti_FA"), file.path(targetDir,"dti_V1"), sep=" ")
            execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
        }
    }
    else
    {
        output(OL$Info, "Running dtifit to do tensor fitting...")
        paramString <- paste("-k", file.path(targetDir,"data"), "-m", file.path(targetDir,"nodif_brain_mask"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(targetDir,"dti"), sep=" ")
        execute("dtifit", paramString, errorOnFail=TRUE)
    }
}

runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0, showOnly = FALSE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!imageFileExists(file.path(targetDir,"nodif")))
        output(OL$Error, "A \"nodif\" image file has not yet been corrected - run eddy_correct first")
    
    if (showOnly)
    {
        paramString <- paste(file.path(targetDir,"nodif"), file.path(targetDir,"nodif_brain_mask"), sep=" ")
        execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
    }
    else
    {
        output(OL$Info, "Running FSL's brain extraction tool...")
        paramString <- paste(file.path(targetDir,"nodif"), file.path(targetDir,"nodif_brain"), "-m -v -f", intensityThreshold, "-g", verticalGradient, sep=" ")
        execute("bet", paramString, errorOnFail=TRUE)
    }
}

runBedpostWithSession <- function (session, nFibres = 2, how = c("fg","bg","screen"), ask = TRUE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    how <- match.arg(how)
    
    targetDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")) || !imageFileExists(file.path(targetDir,"data")) || !imageFileExists(file.path(targetDir,"nodif_brain_mask")))
        output(OL$Error, "Some required files are missing - run eddy_correct and bet first")
    
    if (!ask)
        ans <- "y"
    else
    {
        execute("bedpostx_datacheck", session$getPreBedpostDirectory(), errorOnFail=TRUE)
        ans <- output(OL$Question, "Okay to start bedpostx [yn]?")
    }
    
    if (tolower(ans) == "y")
    {
        unlink(session$getBedpostDirectory(), recursive=TRUE)
        
        if (how == "screen")
        {
            output(OL$Info, "Starting bedpostx in a \"screen\" session")
            bedpostLoc <- locateExecutable("bedpostx", errorIfMissing=TRUE)
            paramString <- paste("-d -m", bedpostLoc, session$getPreBedpostDirectory(), "-n", nFibres, sep=" ")
            execute("screen", paramString, errorOnFail=TRUE)
        }
        else
        {
            output(OL$Info, "Starting bedpostx as a ", ifelse(how=="fg","normal foreground","background"), " process")
            paramString <- paste(session$getPreBedpostDirectory(), "-n", nFibres, sep=" ")
            execute("bedpostx", paramString, errorOnFail=TRUE, wait=(how=="fg"))
        }
    }
}
