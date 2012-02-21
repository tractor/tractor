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
    waypointType <- getConfigVariable("WaypointType", "binary", validValues=c("binary","coded"))
    waypointMode <- getConfigVariable("WaypointMode", "all", validValues=c("all","pairs"))
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
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
    else if (waypointType == "binary")
    {
        waypointMasks <- list()
        for (waypointFile in waypointMaskFiles)
        {
            waypointMask <- newMriImageFromFile(waypointFile)
            if (waypointMasksInStandardSpace)
                waypointMask <- transformStandardSpaceImage(session, waypointMask)
            waypointMasks <- c(waypointMasks, list(waypointMask))
        }
        waypointNumbers <- seq_along(waypointMasks)
    }
    else
    {
        if (length(waypointMaskFiles) > 1)
            report(OL$Error, "Only one waypoint mask may be specified if it is region-coded")
        
        waypointMasks <- list()
        waypointMask <- newMriImageFromFile(waypointMaskFiles)
        waypointNumbers <- unique(as.vector(waypointMask$getData()))
        for (waypointNumber in waypointNumbers)
            waypointMasks <- c(waypointMasks, list(newMriImageWithSimpleFunction(waypointMask, function (i) ifelse(i==waypointNumber,1,0))))
    }
    
    if (is.null(waypointMasks))
        combinations <- matrix(0, nrow=1, ncol=1)
    else if (waypointMode == "all")
        combinations <- matrix(seq_along(waypointNumbers), ncol=1)
    else if (waypointMode == "pairs")
        combinations <- combn(length(waypointNumbers), 2)
    
    if (tracker == "tractor")
    {
        require(tractor.native)
        result <- trackWithSession(session, seedMask, requireImage=is.null(waypointMasks), requireStreamlines=!is.null(waypointMasks), nSamples=nSamples)
    }
    
    for (i in 1:ncol(combinations))
    {
        if (tracker == "fsl")
            result <- runProbtrackWithSession(session, mode="seedmask", seedMask=seedMask, waypointMasks=(if (is.null(waypointMasks)) NULL else waypointMasks[combinations[,i]]), requireImage=TRUE, nSamples=nSamples)
        else if (!is.null(waypointMasks))
        {
            # This is slightly fragile - don't replace streamlines in the result
            streamlines <- newStreamlineCollectionTractWithWaypointConstraints(result$streamlines, waypointMasks[combinations[,i]])
            result$image <- newMriImageAsVisitationMap(streamlines)
            result$nSamples <- streamlines$nStreamlines()
        }
    
        report(OL$Info, "Creating tract images")
        currentTractName <- ifelse(ncol(combinations)==1, tractName, paste(tractName,implode(waypointNumbers[combinations[,i]],sep="_"),sep="_"))
        if (createVolumes)
            writeMriImageToFile(result$image, currentTractName)
        if (createImages)
            writePngsForResult(result, prefix=currentTractName, threshold=vizThreshold, showSeed=showSeed)
    }
    
    invisible (NULL)
}
