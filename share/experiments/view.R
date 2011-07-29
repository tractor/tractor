#@args image file
#@desc Display an image file in "fslview", writing it first into Analyze format to avoid problems with the viewer and certain file datatypes.
#@interactive TRUE

runExperiment <- function ()
{
    requireArguments("image file")
    fileNames <- showImagesInFslview(Arguments[1], writeToAnalyzeFirst=TRUE)
    
    ask("Press Enter to exit:")
    
    for (fileName in fileNames)
        removeImageFilesWithName(fileName)
    
    invisible(NULL)
}
