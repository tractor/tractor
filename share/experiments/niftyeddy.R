#@args [session directory]
#@desc Perform eddy current distortion correction using NiftyReg rather than FLIRT for the specified session directory (default "."). Note that although this largely replaces the results of "preproc" stage 2, that stage must have been run first, so that the T2-weighted reference image has already been chosen.

library(RNiftyReg)
library(oro.nifti)
library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    targetDir <- session$getPreBedpostDirectory()
    
    nLevels <- getConfigVariable("NumberOfLevels", 2, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 5, "integer")
    useBlockPercentage <- getConfigVariable("UseBlockPercentage", 50, "integer")
    useMask <- getConfigVariable("UseTargetMask", FALSE)
    interpolationType <- getConfigVariable("InterpolationType", "cubicspline", validValues=c("nearestneighbour","trilinear","cubicspline"))
    
    report(OL$Info, "Reading source data")
    data <- readNIfTI(file.path(targetDir, "basic"))
    reference <- readNIfTI(file.path(targetDir, "nodif"))
    
    if (useMask)
        targetMask <- readNIfTI(session$getImageFileNameByType("mask"))
    else
        targetMask <- NULL
    interpolationType <- switch(interpolationType, nearestneighbour=0, trilinear=1, cubicspline=3)
    
    finalArray <- array(0, dim=dim(data))
    
    report(OL$Info, "Coregistering data to reference volume")
    for (i in seq_len(dim(data)[4]))
    {
        report(OL$Verbose, "Extracting and registering volume ", i)
        volume <- data[,,,i]
        volume <- as.nifti(volume, data)
        result <- niftyreg(volume, reference, targetMask=targetMask, nLevels=nLevels, maxIterations=maxIterations, useBlockPercentage=useBlockPercentage, finalInterpolation=interpolationType)
        finalArray[,,,i] <- as.array(result$image)
    }
    
    report(OL$Info, "Writing corrected data")
    finalImage <- as.nifti(finalArray, data)
    writeNIfTI(finalImage, file.path(targetDir, "data"))
}
