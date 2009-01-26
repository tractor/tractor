#@args session directory, seed point
#@desc Run single seed point tractography using the specified session and seed. The
#@desc seed point may be specified in standard or native diffusion space and using
#@desc FSL (voxel coordinates, beginning at zero), R (voxel coordinates, beginning at
#@desc one) or mm (world coordinates) conventions. The PointType and SeedInMNISpace
#@desc options need to be set to reflect these choices. Standard space seed points
#@desc are transferred to native space using the FLIRT registration algorithm. The
#@desc tract produced by FSL ProbTrack may be visualised in projection by giving
#@desc CreateImages:true.

suppressPackageStartupMessages(require(tractor.session))

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
        thresholdImage <- session$getImageByType(tolower(thresholdType))
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
