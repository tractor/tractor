#@args session directory
#@desc Run tractography using a mask, as with the "mtrack" experiment, except that
#@desc each nonzero voxel in the mask will generate a SEPARATE output volume (with
#@desc CreateVolumes:true) and/or projection image (CreateImages:true). This
#@desc experiment can therefore generate a very large number of files.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getConfigVariable("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getConfigVariable("SeedMaskInStandardSpace", FALSE)
    thresholdType <- getConfigVariable("SeedThresholdType", "FA", validValues=c("FA","MD","axialdiff","radialdiff"), deprecated=TRUE)
    thresholdLevel <- getConfigVariable("SeedThresholdLevel", NULL, "numeric", errorIfInvalid=TRUE, deprecated=TRUE)
    newThresholdLevel <- getConfigVariable("AnisotropyThreshold", 0.2)
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    
    # The new threshold level takes priority if it differs from the old one
    if (thresholdLevel != newThresholdLevel)
        thresholdLevel <- newThresholdLevel
    
    createVolumes <- getConfigVariable("CreateVolumes", FALSE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    tractName <- getConfigVariable("TractName", "tract")
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    seedMask <- newMriImageFromFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformStandardSpaceImage(session, seedMask)
    
    seeds <- which(seedMask$getData() > 0, arr.ind=TRUE)
    if (nrow(seeds) == 0)
        report(OL$Error, "Seed mask is empty")
    else
    {
        if (is.null(thresholdLevel))
            validSeeds <- 1:nrow(seeds)
        else
        {
            thresholdImage <- session$getImageByType(tolower(thresholdType), "diffusion")
            validSeeds <- which(thresholdImage[seeds] >= thresholdLevel)
            report(OL$Info, "Rejecting ", nrow(seeds)-length(validSeeds), " seed points as below threshold")
        }
        
        runProbtrackWithSession(session, seeds[validSeeds,], requireFile=TRUE, nSamples=nSamples)
        for (d in validSeeds)
        {
            prefix <- paste(tractName, implode(seeds[d,],sep="_"), sep="_")
            ptResult <- runProbtrackWithSession(session, seeds[d,], requireImage=TRUE, nSamples=nSamples, expectExists=TRUE)
            
            if (createVolumes)
                writeMriImageToFile(ptResult$image, prefix)
            if (createImages)
                writePngsForResult(ptResult, prefix=prefix, threshold=vizThreshold, showSeed=showSeed)
        }
    }
    
    invisible (NULL)
}
