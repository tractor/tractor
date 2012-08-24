#@args image file(s)
#@desc Display one or more image files in "fslview". This wrapper script will automatically work around a problem with the viewer and certain file datatypes.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file(s)")
    do.call(showImagesInFslview, c(as.list(Arguments),list(wait=TRUE)))    
    invisible(NULL)
}
