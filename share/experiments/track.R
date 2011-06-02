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
    
    seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    if (!exists("seed") || length(seed) != 3)
        report(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    isStandardSeed <- getConfigVariable("SeedInMNISpace", FALSE)
    
    tracker <- getConfigVariable("Tracker", "fsl", validValues=c("fsl","tractor"))
    
    useGradientAscent <- getConfigVariable("UseGradientAscent", FALSE)
    thresholdType <- getConfigVariable("GradientAscentThresholdType", "fa")
    thresholdLevel <- getConfigVariable("GradientAscentThresholdLevel", 0.2)
    
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    force <- getConfigVariable("Force", FALSE)
    
    createVolumes <- getConfigVariable("CreateVolumes", TRUE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    tractName <- getConfigVariable("TractName", "tract")
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (!createVolumes && !createImages)
        report(OL$Error, "One of \"CreateVolumes\" and \"CreateImages\" must be true")
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    
    if (useGradientAscent)
    {
        thresholdImage <- session$getImageByType(tolower(thresholdType), "diffusion")
        currentSeed <- seed
        currentValue <- thresholdImage$getDataAtPoint(seed)
        
        while (currentValue < thresholdLevel)
        {
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=3)
            nextLoc <- which.max(thresholdImage[t(neighbourhood$vectors)])
            
            if (equivalent(neighbourhood$vectors[,nextLoc], currentSeed))
            {
                report(OL$Warning, "Dead end reached")
                break
            }
            else
            {
                currentSeed <- neighbourhood$vectors[,nextLoc]
                currentValue <- thresholdImage$getDataAtPoint(currentSeed)
            }
            
            report(OL$Verbose, "Ascending to voxel ", implode(currentSeed,","), " with ", toupper(thresholdType), " value ", currentValue)
        }
        
        seed <- currentSeed
    }
    
    report(OL$Info, "Using seed point ", implode(seed,","), " for tractography")
    if (tracker == "fsl")
        result <- runProbtrackWithSession(session, seed, mode="simple", requireImage=TRUE, nSamples=nSamples, force=force)
    else
    {
        require("tractor.native")
        result <- trackWithSession(session, seed, requireImage=TRUE, nSamples=nSamples)
    }
        
    report(OL$Info, "Creating tract images")
    if (createVolumes)
        writeMriImageToFile(result$image, tractName)
    if (createImages)
        writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
    
    invisible (NULL)
}
