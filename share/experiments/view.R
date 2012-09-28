#@args image file(s)
#@desc Display one or more image files. The viewer used will be taken from the TRACTOR_VIEWER environment variable if it is not specified explicitly, and will default to "fslview" if this is not set. This wrapper script will automatically work around a problem with "fslview" and certain file datatypes.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file(s)")
    
    viewer <- tolower(Sys.getenv("TRACTOR_VIEWER"))
    if (!(viewer %in% c("fslview","freeview")))
    {
        if (viewer != "")
            report(OL$Warning, "The \"TRACTOR_VIEWER\" environment variable has an invalid value")
        viewer <- "fslview"
    }
    
    requestedViewer <- getConfigVariable("Viewer", NULL, "character", validValues=c("fslview","freeview"))
    
    if (!is.null(requestedViewer))
        viewer <- requestedViewer
    
    if (viewer == "fslview")
        do.call(showImagesInFslview, c(as.list(Arguments),list(wait=TRUE)))
    else if (viewer == "freeview")
        do.call(showImagesInFreeview, c(as.list(Arguments),list(wait=TRUE)))
    
    invisible(NULL)
}
