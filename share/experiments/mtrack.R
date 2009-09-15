#@args session directory
#@desc Run tractography using one or more masks. Every nonzero voxel in the specified
#@desc SeedMaskFile will be used as a seed point for NumberOfSamples streamlines, and
#@desc the results combined. If any WaypointMaskFiles are specified, streamlines which
#@desc do not pass through ALL of the masks given will be ignored.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getWithDefault("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getWithDefault("SeedMaskInStandardSpace", FALSE)
    waypointMaskFiles <- getWithDefault("WaypointMaskFiles", NULL, "character", errorIfInvalid=TRUE)
    waypointMasksInStandardSpace <- getWithDefault("WaypointMasksInStandardSpace", FALSE)
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    
    tractName <- getWithDefault("TractName", "tract")
    createVolumes <- getWithDefault("CreateVolumes", TRUE)
    createImages <- getWithDefault("CreateImages", FALSE)
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        output(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    seedMask <- newMriImageFromFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformStandardSpaceImage(session, seedMask)
    
    if (is.null(waypointMaskFiles))
        result <- runProbtrackWithSession(session, mode="seedmask", seedMask=seedMask, requireImage=TRUE, nSamples=nSamples)
    else
    {
        waypointMasks <- list()
        for (waypointFile in waypointMaskFiles)
        {
            waypointMask <- newMriImageFromFile(waypointFile)
            if (waypointMasksInStandardSpace)
                waypointMask <- transformStandardSpaceImage(session, waypointMask)
            waypointMasks <- c(waypointMasks, list(waypointMask))
        }
        
        result <- runProbtrackWithSession(session, mode="seedmask", seedMask=seedMask, waypointMasks=waypointMasks, requireImage=TRUE, nSamples=nSamples)
    }
    
    output(OL$Info, "Creating tract images")
    if (createVolumes)
        writeMriImageToFile(result$image, tractName)
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    
    invisible (NULL)
}
