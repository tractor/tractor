runFreesurferForSession <- function (session, options = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    session$unlinkDirectory("freesurfer")
    runWorkflow("reconall", session, FreesurferOptions=implode(options," "))
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
