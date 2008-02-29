runEddyCorrectWithSession <- function (session, ask = FALSE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")))
        output(OL$Error, "The specified session does not contain \"bvals\" and \"bvecs\" files")
    bvals <- unlist(read.table(file.path(targetDir, "bvals")))
    if (min(bvals) != 0)
        output(OL$Info, "Minimal b-value in this data set is ", min(bvals), " rather than 0")
    
    zeroes <- which(bvals == min(bvals))
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
                execute("fslview", file.path(targetDir,"basic"), errorOnFail=TRUE, wait=FALSE)
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

runDtifitWithSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(targetDir,"bvals")) || !file.exists(file.path(targetDir,"bvecs")) || !imageFileExists(file.path(targetDir,"data")) || !imageFileExists(file.path(targetDir,"nodif_brain_mask")))
        output(OL$Error, "Some required files are missing - run eddy_correct and bet first")
    
    output(OL$Info, "Running dtifit to do tensor fitting...")
    paramString <- paste("-k", file.path(targetDir,"data"), "-m", file.path(targetDir,"nodif_brain_mask"), "-r", file.path(targetDir,"bvecs"), "-b", file.path(targetDir,"bvals"), "-o", file.path(targetDir,"dti"), sep=" ")
    execute("dtifit", paramString, errorOnFail=TRUE)
}

runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0, show = FALSE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    targetDir <- session$getPreBedpostDirectory()
    if (!imageFileExists(file.path(targetDir,"nodif")))
        output(OL$Error, "A \"nodif\" image file has not yet been corrected - run eddy_correct first")
    
    output(OL$Info, "Running FSL's brain extraction tool...")
    paramString <- paste(file.path(targetDir,"nodif"), file.path(targetDir,"nodif_brain"), "-m -v -f", intensityThreshold, "-g", verticalGradient, sep=" ")
    execute("bet", paramString, errorOnFail=TRUE)
    
    if (show)
    {
        paramString <- paste(file.path(targetDir,"nodif"), file.path(targetDir,"nodif_brain_mask"), sep=" ")
        execute("fslview", paramString, errorOnFail=TRUE, wait=FALSE)
    }
}

runBedpostWithSession <- function (session, nFibres = 2, how = c("auto","screen","bg","fg"), ask = TRUE)
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
        
        screenAvailable <- !is.null(locateExecutable("screen", errorIfMissing=FALSE))
        if (how == "screen" || (how == "auto" && screenAvailable))
        {
            output(OL$Info, "Starting bedpostx in a \"screen\" session")
            bedpostLoc <- locateExecutable("bedpostx", errorIfMissing=TRUE)
            paramString <- paste("-d -m", bedpostLoc, session$getPreBedpostDirectory(), "-n", nFibres, sep=" ")
            execute("screen", paramString, errorOnFail=TRUE)
        }
        else if (how %in% c("auto","bg"))
        {
            output(OL$Info, "Starting bedpostx as a background process")
            paramString <- paste(session$getPreBedpostDirectory(), "-n", nFibres, sep=" ")
            execute("bedpostx", paramString, errorOnFail=TRUE, wait=FALSE)
        }
        else
        {
            output(OL$Info, "Starting bedpostx as a normal foreground process")
            paramString <- paste(session$getPreBedpostDirectory(), "-n", nFibres, sep=" ")
            execute("bedpostx", paramString, errorOnFail=TRUE)
        }
    }
}
