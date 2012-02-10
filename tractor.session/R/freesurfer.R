runFreesurferForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    freesurferDirectory <- session$getDirectory("freesurfer")
    imagesDirectory <- file.path(freesurferDirectory, "mri", "orig")
    
    subjectsDirectory <- Sys.getenv("SUBJECTS_DIR")
    if (subjectsDirectory == "")
        report(OL$Error, "Freesurfer does not seem to be fully set up: can't identify subjects directory")
    
    nT1wImages <- getImageCountForSession(session, "t1", "structural")
    if (nT1wImages == 0)
        report(OL$Error, "No T1-weighted images are currently stored in the session directory")
    
    if (file.exists(freesurferDirectory))
        unlink(freesurferDirectory, recursive=TRUE)
    
    dir.create(imagesDirectory, recursive=TRUE)
    
    report(OL$Info, "Reading ", nT1wImages, " T1-weighted image(s) and converting to MGH format")
    for (i in 1:nT1wImages)
    {
        image <- session$getImageByType("t1", "structural", index=i)
        targetFileName <- file.path(imagesDirectory, sprintf("%03d",i))
        writeMriImageToFile(image, targetFileName, "MGH_GZ")
    }
    
    subjectName <- basename(session$getDirectory())
    file.symlink(freesurferDirectory, file.path(subjectsDirectory,subjectName))
    
    report(OL$Info, "Running Freesurfer (recon-all)...")
    reconAllArguments <- paste("-subjid", subjectName, "-all", sep=" ")
    execute("recon-all", reconAllArguments)
    
    unlink(file.path(subjectsDirectory,subjectName))
}

showImagesInFreeview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    validColourMaps <- c("grayscale","lut","heat","jet","gecolor","nih")
    
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        valid <- lookupTable %in% validColourMaps
        if (any(!valid))
            report(OL$Warning, "Lookup table name(s) ", implode(unique(lookupTable[!valid]),sep=", ",finalSep=" and "), " are not valid for freeview")
        
        imageFileNames <- paste(imageFileNames, ":colormap=", lookupTable, sep="")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, ":opacity=", opacity, sep="")
    }
    
    execute("freeview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}
