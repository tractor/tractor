#@args image file
#@desc Display an image file in "fslview", writing it first into Analyze format to avoid problems with the viewer and certain file datatypes.
#@interactive TRUE

runExperiment <- function ()
{
    requireArguments("image file")
    
    image <- newMriImageFromFile(Arguments[1])
    
    fileStem <- tempfile()
    writeMriImageToFile(image, fileStem, fileType="ANALYZE_GZ")
    execute("fslview", fileStem, wait=FALSE)
    
    report(OL$Question, "Press Enter to exit:")
    
    removeImageFilesWithName(fileStem)
    invisible(NULL)
}
