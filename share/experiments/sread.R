#@args session directory, image files or DICOM directories
#@desc Read structural images from DICOM, Analyze, NIfTI or MGH files and copy into the specified session directory. If any of the specified paths point to a directory they will be assumed to contain DICOM files. Images may be T1, T2 or proton density weighted.

runExperiment <- function ()
{
    requireArguments("session directory", "image file or DICOM directory")
    
    session <- newSessionFromDirectory(Arguments[1])
    nImages <- nArguments() - 1
    
    weighting <- getConfigVariable("ImageWeighting", "t1", validValues=c("t1","t2","pd"))
    
    currentIndex <- getImageCountForSession(session, weighting, "structural") + 1
    for (i in seq_len(nImages))
    {
        report(OL$Info, "Copying image ", i, " of ", nImages)
        
        if (file.info(Arguments[i+1]$isdir))
            image <- newMriImageFromDicomDirectory(Arguments[i+1])
        else if (!imageFileExists(Arguments[i+1]))
        {
            report(OL$Warning, "Complete image file does not exist: \"", Arguments[i+1], "\"")
            image <- NULL
        }
        else
            image <- newMriImageFromFile(Arguments[i+1])
        
        if (!is.null(image))
        {
            writeMriImageToFile(image, session$getImageFileNameByType(weighting,"structural",index=currentIndex))
            currentIndex <- currentIndex + 1
        }
    }
}
