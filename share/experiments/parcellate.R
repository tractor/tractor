#@args session directory, segmentation images
#@desc Read and merge together parcellations for a T1-weighted image. Labels in segmentations specified later in the command will take priority over duplicates appearing in earlier segmentations. The special symbol '@' can be used to indicate that the session hierarchy should be checked for the parcellation in question, which is useful in combination with the "freesurf" script. Any existing parcellation will be taken as a starting point unless IgnoreExisting:true is given. A region description file in the format used in $TRACTOR_HOME/etc/parcellations must be provided for any parcellation type which is not specified there.

library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "segmentation images")
    
    types <- getConfigVariable("Types", NULL, "character", errorIfMissing=TRUE)
    ignoreExisting <- getConfigVariable("IgnoreExisting", FALSE)
    freesurferSpaceReference <- getConfigVariable("FreesurferSpaceReference", NULL, "character")
    
    session <- newSessionFromDirectory(Arguments[1])
    
    segmentationFiles <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    if (!is.null(types))
        types <- splitAndConvertString(types, ",", fixed=TRUE)
    if (length(segmentationFiles) != length(types))
        report(OL$Error, "A type must be specified for each segmentation file")
    
    # Freesurfer parcellations are defined in its own standardised space, so we need a reference image in that space
    if (any(types %in% c("desikan-killiany","destrieux")))
    {
        if (!is.null(freesurferSpaceReference))
        {
            report(OL$Info, "Reading Freesurfer reference image and registering to the session's T1w image")
            freesurferSpaceImage <- readImageFile(freesurferSpaceReference)
            t1Image <- session$getImageByType("reft1", "structural")
            result <- registerImages(freesurferSpaceImage, t1Image, estimateOnly=TRUE)
            freesurferTransform <- result$transform
        }
        else
            freesurferTransform <- session$getTransformation("freesurfer", "structural")
    }
    
    parcellation <- NULL
    if (!ignoreExisting && session$imageExists("parcellation","structural"))
    {
        report(OL$Info, "Reading initial parcellation")
        parcellation <- readParcellation(session$getImageFileNameByType("parcellation","structural"))
    }
    
    for (i in seq_along(segmentationFiles))
    {
        report(OL$Info, "Reading and merging parcellation #{i} of #{length(segmentationFiles)}...")
        
        regionFileName <- ensureFileSuffix(types[i], "txt")
        if (file.exists(regionFileName))
            regionFilePath <- regionFileName
        else if (file.exists(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "parcellations", regionFileName)))
            regionFilePath <- file.path(Sys.getenv("TRACTOR_HOME"), "etc", "parcellations", regionFileName)
        else
            report(OL$Error, "No region description file was found for parcellation type \"#{types[i]}\"")
        
        if (segmentationFiles[i] == "@")
            currentParcellation <- readParcellation(session$getImageFileNameByType(types[i]), regionFilePath)
        else
            currentParcellation <- readParcellation(segmentationFiles[i], regionFilePath)
        
        if (types[i] %in% c("desikan-killiany","destrieux"))
        {
            report(OL$Info, "Transforming Freesurfer image back to the original T1w space")
            currentParcellation$image <- transformImage(freesurferTransform, currentParcellation$image, finalInterpolation=0)
        }
        
        if (is.null(parcellation))
            parcellation <- currentParcellation
        else
        {
            allCurrentLabels <- splitAndConvertString(currentParcellation$regions$label, ",", fixed=TRUE)
            duplicates <- which(sapply(parcellation$regions$label, function(x) any(splitAndConvertString(x,",",fixed=TRUE) %in% allCurrentLabels)))
            if (length(duplicates) > 0)
            {
                # Order is important here, since the duplicates vector will be wrong if the lookup table is modified first
                parcellation$image <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x %in% parcellation$regions$index[duplicates], 0, x))
                parcellation$regions <- parcellation$regions[-duplicates,]
            }
            
            if (any(currentParcellation$regions$index %in% parcellation$regions$index))
            {
                delta <- max(parcellation$regions$index)
                currentParcellation$regions$index <- currentParcellation$regions$index + delta
            }
            else
                delta <- 0
            
            parcellation$image <- newMriImageWithBinaryFunction(parcellation$image, currentParcellation$image, function(x,y) ifelse(y==0,x,y+delta))
            parcellation$regions <- rbind(parcellation$regions, currentParcellation$regions)
        }
    }
    
    report(OL$Info, "Writing out final parcellation")
    parcellation$regions <- parcellation$regions[order(parcellation$regions$index),]
    writeParcellation(parcellation$image, parcellation$regions, session$getImageFileNameByType("parcellation","structural"))
}
