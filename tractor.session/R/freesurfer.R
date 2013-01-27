showImagesInFreeview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    validColourMaps <- c("grayscale","lut","heat","jet","gecolor","nih")
    
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        valid <- lookupTable %in% validColourMaps
        imageFileNames[valid] <- paste(imageFileNames[valid], ":colormap=", lookupTable[valid], sep="")
        imageFileNames[!valid] <- paste(imageFileNames[!valid], ":color=", lookupTable[!valid], sep="")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, ":opacity=", opacity, sep="")
    }
    
    execute("freeview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    # If we're not waiting for freeview we can't delete the images yet
    if (wait)
        unlink(tempDir, recursive=TRUE)
    
    invisible(unlist(imageFileNames))
}
