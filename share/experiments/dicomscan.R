#@args [directories]

library(RNifti)
library(divest)
library(tractor.base)

runExperiment <- function ()
{
    interactive <- getConfigVariable("Interactive", TRUE)
    flipY <- getConfigVariable("FlipAP", TRUE)
    crop <- getConfigVariable("Crop", FALSE)
    forceStack <- getConfigVariable("AlwaysStack", FALSE)
    thresholdTR <- getConfigVariable("ThresholdTR", 0.5)
    filenameStyle <- getConfigVariable("Filenames", "folder", "character", validValues=c("folder","metadata","both"))
    createSession <- getConfigVariable("CreateSession", FALSE)
    anonymise <- getConfigVariable("Anonymise", FALSE)
    
    session <- attachMriSession(".")
    getSessionPath <- function (type)
    {
        if (tolower(type) %in% c("t1","t2","pd"))
        {
            n <- getImageCountForSession(session, type)
            session$getImageFileNameByType(type, index=n)
        }
        else
            session$getImageFileNameByType("rawdata", type)
    }
    
    if (nArguments() == 0)
        paths <- "."
    else
        paths <- Arguments
    
    for (path in paths)
    {
        report(OL$Info, "Looking for DICOM files in directory #{path}...")
        result <- readDicom(path, interactive=interactive, flipY=flipY, crop=crop, forceStack=forceStack, labelFormat=ifelse(anonymise,"%t_S%3s_%d","%n_%t_S%3s_%d"))
        
        outputPaths <- switch(filenameStyle, folder=rep(basename(path), length(result)),
                                             metadata=sapply(result, as.character),
                                             both=paste(basename(path), sapply(result,as.character), sep="_"))
        
        if (any(duplicated(outputPaths)))
        {
            outputPaths <- unlist(tapply(outputPaths, outputPaths, function(x) {
                if (length(x) == 1)
                    x
                else
                    paste(x, seq_along(x), sep="_")
            }))
        }
        
        for (i in seq_along(result))
        {
            image <- result[[i]]
            
            # TR correction for 4D volumes
            if (ndim(image) == 4 && dim(image)[4] > 1)
            {
                storedTR <- pixdim(image)[4]
                if (storedTR > 0 && storedTR < thresholdTR)
                {
                    report(OL$Info, "Reconstructed TR of #{storedTR} s is less than threshold", round=3)
                    throughPlaneAxis <- abs(tractor.base:::xformToOrientation(xform(image), string=FALSE)[3])
                    image <- as.array(image)
                    pixdim(image)[4] <- pixdim(image)[4] * dim(image)[throughPlaneAxis]
                }
            }
            
            attributes <- attributes(image)
            type <- ""
            
            if (all(c("bValues","bVectors") %in% names(attributes)))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be diffusion-weighted")
                type <- "diffusion"
            }
            else if (ndim(image) == 4 && dim(image)[4] > 9)
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be a BOLD time series")
                type <- "functional"
            }
            else if (isTRUE(attributes$repetitionTime < 500) && isTRUE(attributes$echoTime < 30))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be T1-weighted")
                type <- "t1"
            }
            else if (isTRUE(attributes$repetitionTime > 2000) && isTRUE(attributes$echoTime > 30))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be T2-weighted")
                type <- "t2"
            }
            else if (isTRUE(attributes$repetitionTime > 2000) && isTRUE(attributes$echoTime < 30))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be proton density-weighted")
                type <- "pd"
            }
            else
                report(OL$Info, "The weighting of image #{outputPaths[i]} is not clear")
            
            if (createSession)
            {
                outputPaths[i] <- getSessionPath(type)
                if (!file.exists(dirname(outputPaths[i])))
                    dir.create(dirname(outputPaths[i]))
            }
            
            writeNifti(image, ensureFileSuffix(outputPaths[i],"nii.gz"))
            
            exclusionPattern <- "^\\.|^(dim|imagedim|pixdim|pixunits|class)$"
            if (anonymise)
                exclusionPattern <- ore(exclusionPattern, "|^patient")
            if (all(c("bValues","bVectors") %in% names(attributes)))
            {
                write.table(cbind(attributes$bVectors,attributes$bValues), ensureFileSuffix(outputPaths[i],"dirs"), row.names=FALSE, col.names=FALSE)
                exclusionPattern <- ore(exclusionPattern, "|^bV(alues|ectors)$")
            }
            
            attributes <- attributes[!(names(attributes) %~% exclusionPattern)]
            if (length(attributes) > 0)
                writeYaml(attributes, ensureFileSuffix(outputPaths[i],"tags"), capitaliseLabels=FALSE)
        }
    }
}
