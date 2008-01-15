suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    seedMaskFile <- getWithDefault("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getWithDefault("SeedMaskInStandardSpace", FALSE)
    thresholdType <- getWithDefault("SeedThresholdType", "fa")
    thresholdLevel <- getWithDefault("SeedThresholdLevel", NULL, "numeric", errorIfInvalid=TRUE)
    
    createVolumes <- getWithDefault("CreateVolumes", FALSE)
    createImages <- getWithDefault("CreateImages", FALSE)
    tractName <- getWithDefault("TractName", "tract")
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    seedMask <- newMriImageFromFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformStandardSpaceImage(session, seedMask)
    
    seeds <- which(seedMask$getData() > 0, arr.ind=TRUE)
    if (nrow(seeds) > 0 && (createVolumes || createImages))
    {
        if (is.null(thresholdLevel))
            validSeeds <- 1:nrow(seeds)
        else
        {
            thresholdImage <- session$getImageByType(thresholdType)
            validSeeds <- which(thresholdImage[seeds] >= thresholdLevel)
            output(OL$Info, "Rejecting ", nrow(seeds)-length(validSeeds), " seed points as below threshold")
        }
        
        runProbtrackWithSession(session, seeds[validSeeds,], requireFile=TRUE)
        for (d in validSeeds)
        {
            prefix <- paste(tractName, implode(seeds[d,],sep="_"), sep="_")
            ptResult <- runProbtrackWithSession(session, seeds[d,], requireImage=TRUE, expectExists=TRUE)
            
            if (createVolumes)
                writeMriImageToFile(ptResult$image, prefix)
            if (createImages)
                writePngsForResult(ptResult, prefix=prefix, threshold=vizThreshold, showSeed=showSeed)
        }
    }
    else
        output(OL$Warning, "Nothing to do: seed mask is zero or you did not request volumes or images")
        
    invisible (NULL)
}
