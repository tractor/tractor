#@args image file(s)
#@desc Display one or more image files. The viewer used will be taken from the TRACTOR_VIEWER environment variable if it is not specified explicitly, and will default to "tractor" if this is not set. Note that TractoR's internal viewer uses the R convention for voxel locations (starting at 1), whereas fslview and freeview use the FSL/C convention (starting at 0). This wrapper script will automatically work around a problem with fslview and certain file datatypes.
#@interactive TRUE
#@nohistory TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file(s)")
    
    requestedViewer <- getConfigVariable("Viewer", NULL, "character", validValues=c("tractor","fslview","freeview"))
    
    if (nArguments() > 1)
        lookupTable <- c("greyscale", rep("heat",nArguments()-1))
    else
        lookupTable <- "greyscale"
    
    viewerArguments <- c(as.list(Arguments), list(wait=TRUE,lookupTable=lookupTable))
    if (!is.null(requestedViewer))
        viewerArguments <- c(viewerArguments, list(viewer=requestedViewer))
    
    do.call(showImagesInViewer, viewerArguments)
    
    invisible(NULL)
}
