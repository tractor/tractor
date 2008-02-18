#@args session directory, seed point

suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- newSessionFromDirectory(Arguments[1])
    
    seed <- as.numeric(unlist(strsplit(Arguments[-1], ",")))
    if (!exists("seed") || length(seed) != 3)
        output(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getWithDefault("PointType", NULL, mode="character", errorIfMissing=TRUE)
    isStandardSeed <- getWithDefault("SeedInMNISpace", FALSE)
    
    useGradientAscent <- getWithDefault("UseGradientAscent", FALSE)
    thresholdType <- getWithDefault("GradientAscentThresholdType", "fa")
    thresholdLevel <- getWithDefault("GradientAscentThresholdLevel", 0.2)
    
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    force <- getWithDefault("Force", FALSE)
    
    createImages <- getWithDefault("CreateImages", FALSE)
    tractName <- getWithDefault("TractName", "tract")
    vizThreshold <- getWithDefault("VisualisationThreshold", 0.01)
    showSeed <- getWithDefault("ShowSeedPoint", TRUE)
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    
    if (useGradientAscent)
    {
        thresholdImage <- session$getImageByType(thresholdType)
        currentSeed <- seed
        currentValue <- thresholdImage$getDataAtPoint(seed)
        
        while (currentValue < thresholdLevel)
        {
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=3)
            nextLoc <- which.max(thresholdImage[t(neighbourhood$vectors)])
            
            if (equivalent(neighbourhood$vectors[,nextLoc], currentSeed))
            {
                output(OL$Warning, "Dead end reached")
                break
            }
            else
            {
                currentSeed <- neighbourhood$vectors[,nextLoc]
                currentValue <- thresholdImage$getDataAtPoint(currentSeed)
            }
            
            output(OL$Verbose, "Ascending to voxel ", implode(currentSeed,","), " with ", toupper(thresholdType), " value ", currentValue)
        }
        
        seed <- currentSeed
    }
    
    output(OL$Info, "Using seed point ", implode(seed,","), " for tractography")
    result <- runProbtrackWithSession(session, seed, mode="simple", requireImage=createImages, nSamples=nSamples, force=force)
    
    if (createImages)
    {
        output(OL$Info, "Creating tract images")
        writeMriImageToFile(result$image, tractName)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    }
    
    invisible (NULL)
}
