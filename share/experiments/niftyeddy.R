#@args [session directory]
#@desc Perform eddy current distortion correction using NiftyReg rather than FLIRT for the specified session directory (default "."). This replaces the results of "dpreproc" stage 4.

library(RNiftyReg)
suppressPackageStartupMessages(library(oro.nifti))
library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    nLevels <- getConfigVariable("NumberOfLevels", 2, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 5, "integer")
    useBlockPercentage <- getConfigVariable("UseBlockPercentage", 50, "integer")
    useMask <- getConfigVariable("UseTargetMask", FALSE)
    interpolationType <- getConfigVariable("InterpolationType", "cubicspline", validValues=c("nearestneighbour","trilinear","cubicspline"))
    
    report(OL$Info, "Reading reference volume")
    metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("rawdata", "diffusion"))
    nVolumes <- metadata$getDimensions()[4]
    reference <- as(session$getImageByType("refb0","diffusion"), "nifti")
    
    if (useMask)
        targetMask <- as(session$getImageByType("mask"), "nifti")
    else
        targetMask <- NULL
    interpolationType <- switch(interpolationType, nearestneighbour=0, trilinear=1, cubicspline=3)
    
    finalArray <- array(0, dim=metadata$getDimensions())
    allAffines <- NULL
    
    report(OL$Info, "Coregistering data to reference volume")
    for (i in seq_len(nVolumes))
    {
        report(OL$Verbose, "Reading and registering volume ", i)
        volume <- as(newMriImageFromFile(session$getImageFileNameByType("rawdata","diffusion"), volumes=i), "nifti")
        result <- niftyreg(volume, reference, targetMask=targetMask, scope="affine", nLevels=nLevels, maxIterations=maxIterations, useBlockPercentage=useBlockPercentage, finalInterpolation=interpolationType)
        finalArray[,,,i] <- as.array(result$image)
        allAffines <- rbind(allAffines, convertAffine(result$affine[[1]],volume,reference,newType="fsl"))
    }
    
    report(OL$Info, "Writing corrected data")
    finalMetadata <- newMriImageMetadataFromTemplate(metadata, datatype=getDataTypeByNiftiCode(16))
    finalImage <- newMriImageWithData(finalArray, finalMetadata)
    writeMriImageToFile(finalImage, session$getImageFileNameByType("data","diffusion"))
    
    write.table(allAffines, file.path(session$getDirectory("fdt"),"data.ecclog"), row.names=FALSE, col.names=FALSE)
    
    invisible(NULL)
}
