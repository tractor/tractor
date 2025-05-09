#@args [directories]
#@desc Create an Analyze/NIfTI/MGH volume by combining data from a set of DICOM files stored in the specified directories (default "."). 3D or 4D images will be created, as appropriate, using file names based on the original folder name or the series metadata. TractoR's own internal DICOM-reading code is used by default, for backwards compatibility, but in this case only the UntileMosaics option is used, and each source directory must contain only one image series. The alternative (and recommended) "divest" back-end is based on the well-established and more robust "dcm2niix" tool, and allows multiple series to be handled without pre-sorting, retention of key metadata in auxiliary .tags files, and heuristic-based automatic session construction. NIfTI-1 is always used as the output format in the latter case.
#@group DICOM handling
#@interactive TRUE

library(RNifti)
library(tractor.base)

runExperiment <- function ()
{
    method <- getConfigVariable("Method", "internal", "character", validValues=c("internal","divest"))
    untileMosaics <- getConfigVariable("UntileMosaics", TRUE)
    interactive <- getConfigVariable("Interactive", TRUE)
    flipY <- getConfigVariable("FlipAP", TRUE)
    crop <- getConfigVariable("Crop", FALSE)
    forceStack <- getConfigVariable("AlwaysStack", FALSE)
    thresholdTR <- getConfigVariable("ThresholdTR", 0.5)
    filenameStyle <- getConfigVariable("FileNames", "folder", "character", validValues=c("folder","metadata","both"))
    createSession <- getConfigVariable("CreateSession", FALSE)
    anonymise <- getConfigVariable("Anonymise", FALSE)
    
    if (createSession)
        session <- attachMriSession(".")
    
    getSessionPath <- function (type)
    {
        if (tolower(type) %in% c("t1","t2","pd"))
        {
            n <- getImageCountForSession(session, type)
            session$getImageFileNameByType(type, index=n+1)
        }
        else
            session$getImageFileNameByType("rawdata", type)
    }
    
    if (nArguments() == 0)
        paths <- "."
    else if (nArguments() == 2 && !file.exists(Arguments[2]))
    {
        paths <- Arguments[1]
        filenameStyle <- "argument2"
    }
    else
        paths <- Arguments
    
    divestVerbosity <- switch(names(getOutputLevel()), Debug=2L, Verbose=0L, -1L)
    
    for (path in expandFileName(paths))
    {
        report(OL$Info, "Looking for DICOM files in directory #{path}...")
        
        if (method == "internal")
        {
            info <- readDicomDirectory(path, method="internal", untileMosaics=untileMosaics)
            reportFlags()
            
            outputPath <- switch(filenameStyle, argument2=Arguments[2], folder=basename(path), report(OL$Error,"Metadata is not available for the internal method"))
            writeImageFile(info$image, outputPath)
            print(info$image)
            next
        }
        
        # From here on the divest back-end is assumed
        result <- divest::readDicom(path, interactive=interactive, flipY=flipY, crop=crop, forceStack=forceStack, labelFormat=ifelse(anonymise,"%t_S%3s_%d","%n_%t_S%3s_%d"), verbosity=divestVerbosity)
        
        # Create initial file names
        outputPaths <- switch(filenameStyle, argument2=rep(Arguments[2], length(result)),
                                             folder=rep(basename(path), length(result)),
                                             metadata=sapply(result, as.character),
                                             both=paste(basename(path), sapply(result,as.character), sep="_"))
        
        # Report putative weightings and update file names if required
        for (i in seq_along(result))
        {
            image <- result[[i]]
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
            else if (isTRUE(attributes$repetitionTime > 1500) && isTRUE(attributes$echoTime > 90))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be T2-weighted")
                type <- "t2"
            }
            else if (isTRUE(attributes$repetitionTime > 1500) && isTRUE(attributes$echoTime < 30))
            {
                report(OL$Info, "Image #{outputPaths[i]} appears to be proton density-weighted")
                type <- "pd"
            }
            else
                report(OL$Info, "The weighting of image #{outputPaths[i]} is not clear")
            
            if (createSession && type != "")
            {
                outputPaths[i] <- getSessionPath(type)
                if (!file.exists(dirname(outputPaths[i])))
                    dir.create(dirname(outputPaths[i]))
            }
        }
        
        # Deduplicate file names
        if (any(duplicated(outputPaths)))
        {
            tapply(seq_along(outputPaths), outputPaths, function(i) {
                if (length(i) > 1)
                {
                    report(OL$Verbose, "Deduplicating output path #{outputPaths[i[1]]}")
                    outputPaths[i] <<- paste(outputPaths[i], seq_along(i), sep="_")
                }
            })
        }
        
        report(OL$Info, "Finalising images and writing them out...")
        for (i in seq_along(result))
        {
            image <- result[[i]]
            attributes <- attributes(image)
            
            # TR correction for 4D volumes
            if (ndim(image) == 4 && dim(image)[4] > 1)
            {
                storedTR <- pixdim(image)[4]
                if (storedTR > 0 && storedTR < thresholdTR)
                {
                    # This section can fail, for example for RGB images, which RNifti does not handle
                    try({
                        throughPlaneAxis <- which(abs(rotation(image))[,3] > 0.5)
                        image <- as.array(image)
                        pixdim(image)[4] <- pixdim(image)[4] * dim(image)[throughPlaneAxis]
                        flag(OL$Info, "Reconstructed TR of #{storedTR} s is less than threshold - correcting to #{pixdim(image)[4]} s", round=3)
                    
                        if ("repetitionTime" %in% names(attributes) && equivalent(attributes$repetitionTime, storedTR*1000, tolerance=1e-4))
                            attributes$repetitionTime <- round(pixdim(image)[4] * 1000)
                    
                        secondaryAttributes <- attributes[!(names(attributes) %~% "^\\.|^(dim|imagedim|pixdim|pixunits|class)$")]
                        if (length(secondaryAttributes) > 0)
                            image <- do.call(structure, c(list(image), secondaryAttributes))
                    })
                }
            }
            
            # Write the image to file
            report(OL$Verbose, "Writing image #{outputPaths[i]}")
            writeImageFile(structure(image,anonymise=anonymise), outputPaths[i], writeTags=TRUE)
        }
    }
}
