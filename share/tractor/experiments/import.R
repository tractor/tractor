#@args session directory, image files or DICOM directories
#@desc Import images from DICOM, Analyze, NIfTI or MGH files, copying them into the appropriate places in a session directory. If any of the specified paths point to a directory they will be assumed to contain DICOM files. Images may be T1, T2, proton density or diffusion weighted, or functional. For diffusion data this is an alternative to stage 1 of the "dpreproc" script, but without the ability to read gradient directions. TractoR does not currently provide facilities for preprocessing functional data, so such images must already be fully preprocessed. Multiple T1-weighted images (only) will be coregistered together and a median reference image created, unless Coregister:false is given.
#@group Working with sessions

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "image files or DICOM directories")
    
    session <- attachMriSession(Arguments[1])
    nImages <- nArguments() - 1
    
    weighting <- getConfigVariable("ImageWeighting", "t1", validValues=c("t1","t2","pd","diffusion","functional"))
    dicomReader <- getConfigVariable("DicomReader", "internal", validValues=c("internal","divest"))
    coregister <- getConfigVariable("Coregister", TRUE)
    
    if (weighting %in% c("diffusion","functional"))
    {
        session$getDirectory(weighting, createIfMissing=TRUE)
        
        images <- lapply(Arguments[-1], function (path) {
            if (isTRUE(file.info(path)$isdir))
                readDicomDirectory(path, method=dicomReader, interactive=FALSE)$image
            else
                readImageFile(path)
        })
        
        if (length(images) == 1)
            image <- images[[1]]
        else
            image <- do.call(mergeMriImages, c(images, list(bindDim=4L, padTags=TRUE)))
        
        assert(image$getDimensionality() == 4L, "A #{weighting} dataset must be four-dimensional")
        
        writeImageFile(image, session$getImageFileNameByType("rawdata",weighting), writeTags=TRUE)
    }
    else
    {
        session$getDirectory("structural", createIfMissing=TRUE)
        currentIndex <- getImageCountForSession(session, weighting, "structural") + 1
        for (i in seq_len(nImages))
        {
            report(OL$Info, "Copying image ", i, " of ", nImages)
        
            if (isTRUE(file.info(Arguments[i+1])$isdir))
                image <- readDicomDirectory(Arguments[i+1], method=dicomReader, interactive=FALSE)$image
            else if (!imageFileExists(Arguments[i+1]))
            {
                report(OL$Warning, "Complete image file does not exist: \"", Arguments[i+1], "\"")
                image <- NULL
            }
            else
                image <- readImageFile(Arguments[i+1])
        
            if (!is.null(image))
            {
                writeImageFile(image, session$getImageFileNameByType(weighting,"structural",index=currentIndex), writeTags=TRUE)
                currentIndex <- currentIndex + 1
            }
        }
    }
    
    if (coregister && weighting == "t1")
    {
        nImages <- getImageCountForSession(session, "t1", "structural")
        if (nImages > 1)
        {
            report(OL$Info, "Coregistering volumes")
            library(tractor.reg)
            
            reference <- session$getImageByType("t1", "structural", index=1)
            data <- array(NA, dim=c(reference$getDimensions(),nImages))
            data[,,,1] <- reference$getData()
            
            for (i in 2:nImages)
            {
                currentImage <- session$getImageByType("t1", "structural", index=i)
                result <- registerImages(currentImage, reference, types="affine", affineDof=6)
                data[,,,i] <- result$transformedImage$getData()
            }
            
            # NB: For n=2, the mean and median are equivalent, but the mean is quicker to calculate
            report(OL$Info, "Calculating voxelwise median image for reference")
            if (nImages == 2)
                finalImage <- asMriImage(apply(data,1:3,mean), reference)
            else
                finalImage <- asMriImage(apply(data,1:3,median), reference)
            
            report(OL$Info, "Writing median image")
            writeImageFile(finalImage, session$getImageFileNameByType("refT1","structural"))
        }
        else
        {
            report(OL$Info, "Symlinking volume to reference")
            symlinkImageFiles(session$getImageFileNameByType("t1","structural",index=1), session$getImageFileNameByType("refT1","structural"))
        }
    }
}
