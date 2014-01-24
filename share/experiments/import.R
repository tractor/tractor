#@args session directory, image files or DICOM directories
#@desc Import images from DICOM, Analyze, NIfTI or MGH files, copying them into the appropriate places in a session directory. If any of the specified paths point to a directory they will be assumed to contain DICOM files. Images may be T1, T2, proton density or diffusion weighted. In the latter case this is an alternative to stage 1 of the "dpreproc" script, but without the ability to read gradient directions. Multiple T1-weighted images (only) will be coregistered together and a median reference image created, unless Coregister:false is given.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "image files or DICOM directories")
    
    session <- newSessionFromDirectory(Arguments[1])
    nImages <- nArguments() - 1
    
    weighting <- getConfigVariable("ImageWeighting", "t1", validValues=c("t1","t2","pd","diffusion"))
    coregister <- getConfigVariable("Coregister", TRUE)
    
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
    
    if (coregister && weighting == "t1")
    {
        nImages <- getImageCountForSession(session, "t1", "structural")
        if (nImages > 1)
        {
            library(tractor.reg)
            
            reference <- session$getImageByType("t1", "structural", index=1)
            data <- array(NA, dim=c(reference$getDimensions(),nImages))
            data[,,,1] <- reference$getData()
            
            for (i in 2:nImages)
            {
                currentImage <- session$getImageByType("t1", "structural", index=i)
                result <- registerImages(currentImage, reference, types="affine", affineDof=6, cache="ignore")
                data[,,,i] <- result$transformedImage$getData()
            }
            
            finalImage <- newMriImageWithData(apply(data,1:3,median), reference)
            writeImageFile(finalImage, session$getImageFileNameByType("reft1","structural"))
        }
        else
            symlinkImageFiles(session$getImageFileNameByType("t1","structural",index=1), session$getImageFileNameByType("reft1","structural"))
    }
}
