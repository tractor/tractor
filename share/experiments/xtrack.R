#@args session directory
#@desc Track the interconnections between target regions, which must have been delineated in a parcellation (typically via "parcellate" or "freesurf"). Target regions may be specified individually or by type, using names from the parcellation's lookup table. Seed regions are specified similarly, or can be left unspecified in which case the whole brain mask will be used. Seed regions may be morphologically modified by erosion, dilation, or using combined operations to find an inner or outer boundary; and an anisotropy threshold can also be applied. Streamlines must enter one of the target regions, and will be terminated as soon as they do. The ParcellationConfidence variable controls the inclusiveness of the transformed parcellation: the closer to 0, the more inclusive.

library(tractor.reg)
library(tractor.session)
library(tractor.track)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedRegions <- getConfigVariable("SeedRegions", NULL, "character")
    boundaryManipulation <- getConfigVariable("BoundaryManipulation", "none", validValues=c("none","erode","dilate","inner","outer"))
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL)
    jitter <- getConfigVariable("JitterSeeds", FALSE)
    targetRegions <- getConfigVariable("TargetRegions", "cerebral_cortex")
    parcellationConfidence <- getConfigVariable("ParcellationConfidence", 0.2)
    nSamples <- getConfigVariable("NumberOfSamples", 100)
    
    tractName <- getConfigVariable("TractName", "tract")
    createVolumes <- getConfigVariable("CreateVolumes", FALSE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    storeStreamlines <- getConfigVariable("StoreStreamlines", TRUE)
    storeTrackingMask <- getConfigVariable("StoreTrackingMask", FALSE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    
    if (!createVolumes && !createImages && !storeStreamlines)
        report(OL$Error, "At least one of \"CreateVolumes\", \"CreateImages\" and \"StoreStreamlines\" must be true")
    
    if (!is.null(seedRegions))
        seedRegions <- splitAndConvertString(seedRegions, ",", fixed=TRUE)
    targetRegions <- splitAndConvertString(targetRegions, ",", fixed=TRUE)
    
    report(OL$Info, "Transforming and reading diffusion-space parcellation")
    parcellation <- session$getParcellation("diffusion", threshold=parcellationConfidence)
    
    if (is.null(seedRegions))
        seedImage <- session$getImageByType("mask", "diffusion")
    else
    {
        seedMatches <- matchRegions(seedRegions, parcellation)
        report(OL$Info, "Using #{length(seedMatches)} matched seed regions")
    
        seedImage <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x %in% seedMatches, 1, 0))
        if (boundaryManipulation != "none")
        {
            report(OL$Info, "Performing boundary manipulation")
        
            library(mmand)
            kernel <- shapeKernel(c(3,3,3), type="box")
            if (boundaryManipulation == "erode")
                seedImage <- newMriImageWithSimpleFunction(seedImage, erode, kernel=kernel)
            else if (boundaryManipulation == "dilate")
                seedImage <- newMriImageWithSimpleFunction(seedImage, dilate, kernel=kernel)
            else if (boundaryManipulation == "inner")
                seedImage <- newMriImageWithSimpleFunction(seedImage, function(x) x - erode(x,kernel=kernel))
            else if (boundaryManipulation == "outer")
                seedImage <- newMriImageWithSimpleFunction(seedImage, function(x) dilate(x,kernel=kernel) - x)
        }
    }
    
    if (!is.null(anisotropyThreshold))
    {
        report(OL$Info, "Applying FA threshold to seed mask")
        fa <- session$getImageByType("FA", "diffusion")
        seedImage <- newMriImageWithBinaryFunction(seedImage, fa, function(x,y) ifelse(y>=anisotropyThreshold,x,0))
    }
    
    targetMatches <- matchRegions(targetRegions, parcellation)
    report(OL$Info, "Using #{length(targetMatches)} matched target regions")
    
    report(OL$Info, "Creating tracking mask")
    mask <- session$getImageByType("mask", "diffusion")
    mask <- newMriImageWithBinaryFunction(mask, parcellation$image, function(x,y) ifelse(y %in% targetMatches, 0, x))
    if (storeTrackingMask)
        maskFileName <- paste(tractName, "tracking_mask", sep="_")
    else
        maskFileName <- threadSafeTempFile()
    writeImageFile(mask, maskFileName)
    
    result <- trackWithSession(session, seedImage, maskName=maskFileName, nSamples=nSamples, requireImage=FALSE, requireStreamlines=TRUE, terminateOutsideMask=TRUE, mustLeaveMask=TRUE, jitter=jitter)
    
    report(OL$Info, "Writing outputs")
    if (storeStreamlines)
        result$streamlines$serialise(paste(tractName,"streamlines",sep="_"))
    if (createVolumes || createImages)
    {
        library(tractor.nt)
        result$image <- newMriImageAsVisitationMap(result$streamlines)
        result$nSamples <- result$streamlines$nStreamlines()
    }
    if (createVolumes)
        writeImageFile(result$image, tractName)
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=FALSE)
}
