#@args session directory
#@desc Run tractography using one or more masks. Every nonzero voxel in the specified SeedMaskFile will be used as a seed point for NumberOfSamples streamlines, and the results combined. If the SeedMaskFile option is not specified, then seeding will be performed throughout the brain mask, subject to any anisotropy threshold specified. If any WaypointMaskFiles are specified, streamlines which do not pass through ALL of the masks given will be ignored.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getConfigVariable("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE)
    seedMaskInStandardSpace <- getConfigVariable("SeedMaskInStandardSpace", FALSE)
    waypointMaskFiles <- getConfigVariable("WaypointMaskFiles", NULL, "character", errorIfInvalid=TRUE)
    waypointMasksInStandardSpace <- getConfigVariable("WaypointMasksInStandardSpace", FALSE)
    tracker <- getConfigVariable("Tracker", "fsl", validValues=c("fsl","tractor"))
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL)
    
    tractName <- getConfigVariable("TractName", "tract")
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    if (is.null(seedMaskFile))
        seedMask <- session$getImageByType("mask", "diffusion")
    else
    {
        seedMask <- newMriImageFromFile(seedMaskFile)
        if (seedMaskInStandardSpace)
            seedMask <- transformStandardSpaceImage(session, seedMask)
    }
    
    if (!is.null(anisotropyThreshold))
    {
        faImage <- session$getImageByType("FA", "diffusion")
        seedMask <- newMriImageWithBinaryFunction(seedMask, faImage, function (x,y) replace(x, y < anisotropyThreshold, 0))
    }
    
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
            if (is.null(streamlines))
            {
                metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("mask","diffusion"))
                result$image <- newMriImageWithData(array(0,dim=metadata$getDimensions()), metadata)
                result$nSamples <- 0
            }
            else
            {
                result$image <- newMriImageAsVisitationMap(streamlines)
                result$nSamples <- streamlines$nStreamlines()
            }
        }
    }
    
    report(OL$Info, "Creating tract images")
    if (createVolumes)
        writeMriImageToFile(result$image, tractName)
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    
    invisible (NULL)
}
