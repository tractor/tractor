require(tractor.fsl)

runExperiment <- function ()
{
    requireArguments(2)
    
    session <- newSessionFromDirectory(Arguments[1])
    if (is.character(Arguments[2]))
    {
        seed <- as.numeric(unlist(strsplit(Arguments[2], ",")))
        if (length(seed) == 1 && is.na(seed))
            output(OL$Error, "Seed point must be comma or space separated")
    }
    else if (is.numeric(Arguments[2]))
        seed <- Arguments[-1]
    
    if (!exists("seed") || length(seed) != 3)
        output(OL$Error, "Seed point must be given as a single vector in 3D space")
    
    seedType <- match.arg(getWithDefault("SeedType", "fsl"), c("fsl","r","mm"))
    isStandardSeed <- getWithDefault("SeedInMNISpace", FALSE)
    
    useGradientAscent <- getWithDefault("UseGradientAscent", FALSE)
    thresholdType <- getWithDefault("GradientDescentThresholdType", "fa")
    thresholdLevel <- getWithDefault("GradientAscentThresholdLevel", 0.2)
    
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    force <- getWithDefault("Force", FALSE)
    
    createImages <- getWithDefault("CreateImages", FALSE)
    tractName <- getWithDefault("TractName", "tract")
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    if (seedType == "fsl")
        seed <- transformFslVoxelToRVoxel(seed)
    if (isStandardSeed)
        seed <- transformStandardSpaceSeeds(session, seed, unit=ifelse(seedType=="mm","mm","vox"))
    else if (seedType == "mm")
    {
        metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("t2"))
        seed <- transformWorldToRVoxel(seed, metadata, useOrigin=TRUE)
    }
    
    seed <- round(seed)
    
    if (useGradientAscent)
    {
        thresholdImage <- session$getImageByType(thresholdType)
        currentSeed <- seed
        currentValue <- thresholdImage$getDataAtPoint(seed)
        
        while (currentValue < thresholdLevel)
        {
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=3)
            nextLoc <- which.max(thresholdImage[t(neighbourhood$vectors)])
            currentSeed <- neighbourhood$vectors[,nextLoc]
            currentValue <- thresholdImage$getDataAtPoint(currentSeed)
            
            output(OL$Verbose, "Ascending to voxel ", implode(currentSeed,","), " with ", toupper(thresholdType), " value ", currentValue)
        }
        
        seed <- currentSeed
    }
    
    output(OL$Info, "Using seed point ", implode(seed,","), " for tractography")
    result <- runProbtrackWithSession(session, seed, mode="simple", requireImage=createImages, nSamples=nSamples, force=force)
    
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    
    invisible (NULL)
}
