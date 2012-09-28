showImagesInFreeview <- function (..., wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    tempDir <- threadSafeTempFile()
    if (!file.exists(tempDir))
        dir.create(tempDir)
    
    imageList <- list(...)
    imageFileNames <- lapply(seq_along(imageList), function (i) {
        if (is.character(imageList[[i]]))
        {
            imageInfo <- identifyImageFileNames(imageList[[i]], errorIfMissing=FALSE)
            if (is.null(imageInfo))
            {
                report(OL$Warning, "Image file \"", imageList[[i]], "\" does not exist")
                return(NULL)
            }
            
            imageLoc <- imageList[[i]]
        }
        else if (is(imageList[[i]], "MriImage"))
        {
            dir.create(file.path(tempDir, i))
            imageLoc <- file.path(tempDir, i, basename(imageList[[i]]$getSource()))
            writeMriImageToFile(imageList[[i]], imageLoc, fileType="NIFTI_GZ")
        }
        else
            report(OL$Error, "Images must be specified as MriImage objects or file names")
        
        return (imageLoc)
    })
    
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, ":colormap=", lookupTable, sep="")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, ":opacity=", opacity, sep="")
    }
    
    execute("freeview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    # If we're not waiting for fslview we can't delete the images yet
    if (wait)
        unlink(tempDir, recursive=TRUE)
    
    invisible(unlist(imageFileNames))
}
