#@args image file(s)
#@desc Display one or more image files in "fslview", writing it first into Analyze format to avoid problems with the viewer and certain file datatypes.
#@interactive TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file(s)")
    do.call(showImagesInFslview, c(as.list(Arguments),list(writeToAnalyzeFirst=TRUE)))
    
    ask("Press Enter to exit:")
    
    invisible(NULL)
}
