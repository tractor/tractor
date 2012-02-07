#@args session directory
#@desc Run tractography using one or more masks. Every nonzero voxel in the specified SeedMaskFile will be used as a seed point for NumberOfSamples streamlines, and the results combined. If any WaypointMaskFiles are specified, streamlines which do not pass through ALL of the masks given will be ignored.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getConfigVariable("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getConfigVariable("SeedMaskInStandardSpace", FALSE)
    waypointMaskFiles <- getConfigVariable("WaypointMaskFiles", NULL, "character", errorIfInvalid=TRUE)
    waypointMasksInStandardSpace <- getConfigVariable("WaypointMasksInStandardSpace", FALSE)
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    
    tractName <- getConfigVariable("TractName", "tract")
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    seedMask <- newMriImageFromFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformStandardSpaceImage(session, seedMask)
    
    if (is.null(waypointMaskFiles))
        waypointMasks <- NULL
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
    }
    
    if (tracker == "fsl")
        result <- runProbtrackWithSession(session, mode="seedmask", seedMask=seedMask, waypointMasks=waypointMasks, requireImage=TRUE, nSamples=nSamples)
    else
    {
        require(tractor.native)
        result <- trackWithSession(session, seedMask, requireImage=is.null(waypointMasks), requireStreamlines=!is.null(waypointMasks), nSamples=nSamples)
        if (!is.null(waypointMasks))
        {
            streamlines <- newStreamlineCollectionTractWithWaypointConstraints(result$streamlines, waypointMasks)
            result$image <- newMriImageAsVisitationMap(streamlines)
            result$nSamples <- streamlines$nStreamlines()
        }
    }
    
    report(OL$Info, "Creating tract images")
    if (createVolumes)
        writeMriImageToFile(result$image, tractName)
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    
    invisible (NULL)
}
