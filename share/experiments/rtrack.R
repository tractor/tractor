#@args session directory
#@desc Run tractography using a mask, as with the "mtrack" experiment, except that each nonzero voxel in the mask will generate a SEPARATE output volume (with CreateVolumes:true) and/or projection image (CreateImages:true). This experiment can therefore generate a very large number of files. With Tracker:tractor the seed points in the mask are taken in groups to avoid memory problems: the number of seeds in each group can be set with the SeedGroupSize option.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getConfigVariable("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getConfigVariable("SeedMaskInStandardSpace", FALSE)
    thresholdType <- getConfigVariable("SeedThresholdType", "FA", validValues=c("FA","MD","axialdiff","radialdiff"), deprecated=TRUE)
    thresholdLevel <- getConfigVariable("SeedThresholdLevel", NULL, "numeric", errorIfInvalid=TRUE, deprecated=TRUE)
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    groupSize <- getConfigVariable("SeedGroupSize", 100)
    newThresholdLevel <- getConfigVariable("AnisotropyThreshold", NULL)
    
    # The new threshold level takes priority if it differs from the old one
    if (is.null(thresholdLevel) || (!is.null(newThresholdLevel) && thresholdLevel != newThresholdLevel))
        thresholdLevel <- newThresholdLevel
    
    createVolumes <- getConfigVariable("CreateVolumes", FALSE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    tractName <- getConfigVariable("TractName", "tract")
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    seedMask <- readImageFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformImageToSpace(seedMask, session, "diffusion", oldSpace="mni", reverseRegister=TRUE)
    
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
        
        if (tracker == "fsl")
        {
            runProbtrackWithSession(session, seeds[validSeeds,], requireFile=TRUE, nSamples=nSamples)
            for (d in validSeeds)
            {
                prefix <- paste(tractName, implode(seeds[d,],sep="_"), sep="_")
                ptResult <- runProbtrackWithSession(session, seeds[d,], requireImage=TRUE, nSamples=nSamples, expectExists=TRUE)
            
                if (createVolumes)
                    writeImageFile(ptResult$image, prefix)
                if (createImages)
                    writePngsForResult(ptResult, prefix=prefix, threshold=vizThreshold, showSeed=showSeed)
            }
        }
        else
        {
            require(tractor.native)
            nGroups <- (length(validSeeds) - 1) %/% groupSize + 1
            for (i in 1:nGroups)
            {
                firstSeed <- groupSize * (i-1) + 1
                lastSeed <- min(i*groupSize, length(validSeeds))
                currentSeeds <- seeds[validSeeds[firstSeed:lastSeed],,drop=FALSE]
                result <- trackWithSession(session, currentSeeds, requireImage=FALSE, requireStreamlines=TRUE, nSamples=nSamples)
                for (j in 1:nrow(currentSeeds))
                {
                    firstStreamline <- nSamples * (j-1) + 1
                    lastStreamline <- j * nSamples
                    streamlinesForSeed <- newStreamlineCollectionTractBySubsetting(result$streamlines, firstStreamline:lastStreamline)
                    imageForSeed <- newMriImageAsVisitationMap(streamlinesForSeed)
                    
                    prefix <- paste(tractName, implode(currentSeeds[j,],sep="_"), sep="_")
                    if (createVolumes)
                        writeImageFile(imageForSeed, prefix)
                    if (createImages)
                    {
                        fakeResult <- list(image=imageForSeed, nSamples=nSamples, session=session, seeds=currentSeeds[j,,drop=FALSE])
                        writePngsForResult(fakeResult, prefix=prefix, threshold=vizThreshold, showSeed=showSeed)
                    }
                }
            }
        }
    }
    
    invisible (NULL)
}
