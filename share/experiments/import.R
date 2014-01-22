#@args session directory, image files or DICOM directories
#@desc Import images from DICOM, Analyze, NIfTI or MGH files, copying them into the appropriate places in a session directory. If any of the specified paths point to a directory they will be assumed to contain DICOM files. Images may be T1, T2, proton density or diffusion weighted. In the latter case this is an alternative to stage 1 of the "dpreproc" script, but without the ability to read gradient directions.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "image files or DICOM directories")
    
    session <- newSessionFromDirectory(Arguments[1])
    nImages <- nArguments() - 1
    
    weighting <- getConfigVariable("ImageWeighting", "t1", validValues=c("t1","t2","pd","diffusion"))
    
    if (weighting == "diffusion")
    {
        if (nImages > 1)
            report(OL$Error, "Only one image may be specified in the case of diffusion data")
        
        session$getDirectory("diffusion", createIfMissing=TRUE)
        
        if (isTRUE(file.info(Arguments[2])$isdir))
            image <- readDicomDirectory(Arguments[2])$image
        else
            image <- readImageFile(Arguments[2])
        
        if (image$getDimensionality() != 4)
            report(OL$Error, "A diffusion dataset must be four-dimensional")
        
        writeImageFile(image, session$getImageFileNameByType("rawdata","diffusion"))
    }
    else
    {
        session$getDirectory("structural", createIfMissing=TRUE)
        currentIndex <- getImageCountForSession(session, weighting, "structural") + 1
        for (i in seq_len(nImages))
        {
            report(OL$Info, "Copying image ", i, " of ", nImages)
        
            if (isTRUE(file.info(Arguments[i+1])$isdir))
                image <- readDicomDirectory(Arguments[i+1])$image
            else if (!imageFileExists(Arguments[i+1]))
            {
                report(OL$Warning, "Complete image file does not exist: \"", Arguments[i+1], "\"")
                image <- NULL
            }
            else
                image <- readImageFile(Arguments[i+1])
        
            if (!is.null(image))
            {
                writeImageFile(image, session$getImageFileNameByType(weighting,"structural",index=currentIndex))
                currentIndex <- currentIndex + 1
            }
        }
    }
}
