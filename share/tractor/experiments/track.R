#@desc Run tractography for a session containing diffusion data, either for the entire seed area at once (Strategy:global) or regionwise or voxelwise. The number of streamlines generated in each case may be given as a literal integer (in which case points are chosen randomly for each streamline) or as an integer followed by "x", in which case that many will be generated for each eligible seed. Seed regions may be voxel locations (given using the R voxel convention), image file names or named regions in a parcellation. If RequirePaths:true is given then streamlines will be saved in TrackVis .trk format. If target regions are also specified then an auxiliary label file with extension .trkl is also created, which maps streamlines onto the targets they reached.
#@args session directory, [seed region(s)]
#@example # Seed everywhere within the brain mask
#@example tractor track /data/subject1
#@example # Seed in all white matter and use cortical grey matter regions as targets
#@example tractor track /data/subject1 white_matter TargetRegions:cerebal_cortex TerminateAtTargets:true
#@example # Seed just outside a particular grey matter region
#@example tractor track /data/subject1 postcentral_gyrus_left BoundaryManipulation:outer
#@group Streamline tractography

library(ore)
library(tractor.track)
library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- attachMriSession(Arguments[1])
    
    strategy <- getConfigVariable("Strategy", "global", validValues=c("global","regionwise","voxelwise"))
    nStreamlines <- getConfigVariable("Streamlines", "100x")
    preferredModel <- getConfigVariable("PreferredModel", "bedpost", validValues=c("bedpost","dti"))
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL, "numeric")
    parcellationConfidence <- getConfigVariable("ParcellationConfidence", 0.2)
    boundaryManipulation <- getConfigVariable("BoundaryManipulation", "none", validValues=c("none","erode","dilate","inner","outer"))
    kernelShape <- getConfigVariable("KernelShape", "diamond", validValues=c("box","disc","diamond"))
    jitter <- getConfigVariable("JitterSeeds", TRUE)
    stepLength <- getConfigVariable("StepLength", 0.5)
    oneWay <- getConfigVariable("OneWay", FALSE)
    targetRegions <- getConfigVariable("TargetRegions", NULL, "character")
    terminateAtTargets <- getConfigVariable("TerminateAtTargets", FALSE)
    minTargetHits <- getConfigVariable("MinTargetHits", "0", "character")
    minLength <- getConfigVariable("MinLength", 0)
    maxLength <- getConfigVariable("MaxLength", Inf)
    tractName <- getConfigVariable("TractName", "tract")
    requireMap <- getConfigVariable("RequireMap", TRUE)
    requireStreamlines <- getConfigVariable("RequirePaths", FALSE)
    requireProfile <- getConfigVariable("RequireProfiles", FALSE)
    
    if (!(nStreamlines %~% "^(\\d+)(x?)$"))
        report(OL$Error, "Number of streamlines should be a positive integer, optionally followed by \"x\"")
    else
    {
        randomSeeds <- is.na(ore.lastmatch()[1,2])
        nStreamlines <- as.integer(ore.lastmatch()[1,1])
    }
    
    seedRegions <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    if (!is.null(targetRegions))
        targetRegions <- splitAndConvertString(targetRegions, ",", fixed=TRUE)
    
    mask <- session$getImageByType("mask", "diffusion")
    
    # If no seed point or region is specified, seeds can be drawn from anywhere in the brain
    if (length(seedRegions) == 0)
        seedInfo <- list(image=mask$copy()$binarise(), indices=1L, labels="brain")
    else if (length(seedRegions) %% 3 == 0 && isValidAs(seedRegions,"integer"))
    {
        seedMatrix <- matrix(as.integer(seedRegions), ncol=3, byrow=TRUE)
        seedImage <- mask$copy()$fill(0L)
        seedImage[seedMatrix] <- 1L
        seedInfo <- list(image=seedImage, indices=1L, labels="points")
    }
    else
        seedInfo <- resolveRegions(seedRegions, session, "diffusion", parcellationConfidence)
    
    if (!is.null(anisotropyThreshold))
    {
        fa <- session$getImageByType("FA")
        seedInfo$image[fa$find("<", anisotropyThreshold)] <- 0L
    }
    
    if (!is.null(targetRegions))
        targetInfo <- resolveRegions(targetRegions, session, "diffusion", parcellationConfidence)
    else
        targetInfo <- list(image=NULL, indices=NULL, labels=NULL)
    
    if (requireProfile && length(targetInfo$indices) == 0)
        report(OL$Error, "")
    
    if (minTargetHits == "all")
        minTargetHits <- length(targetInfo$indices)
    else if (!isValidAs(minTargetHits, "integer"))
        report(OL$Error, "MinTargetHits must be an integer or \"all\"")
    else
        minTargetHits <- as.integer(minTargetHits)
    
    tracker <- session$getTracker(mask, preferredModel=preferredModel, stepLength=stepLength, oneWay=oneWay)
    tracker$setTargets(targetInfo, terminate=terminateAtTargets)
    report(OL$Info, "Using #{toupper(tracker$getModel()$getType())} diffusion model for #{strategy} tractography")
    
    profiles <- list()
    processStreamlines <- function (streamSource, fileStem)
    {
        streamSource$filter(minLabels=minTargetHits, minLength=minLength, maxLength=maxLength)
        result <- streamSource$process(fileStem, requireStreamlines=requireStreamlines, requireMap=requireMap, requireProfile=requireProfile)
        if (!is.null(result$map))
            writeImageFile(result$map, fileStem)
        return (result$profile)
    }
    
    startTime <- Sys.time()
    
    # Iterate over seed regions
    for (index in seedInfo$indices)
    {
        outputLevel <- ifelse(length(seedInfo$indices)==1, OL$Info, OL$Verbose)
        
        seedImage <- seedInfo$image$map(fx(ifelse(x==index, 1L, 0L)))
        if (boundaryManipulation != "none")
        {
            kernel <- mmand::shapeKernel(c(3,3,3), type=kernelShape)
            if (boundaryManipulation == "erode")
                seedImage$map(mmand::erode, kernel=kernel)
            else if (boundaryManipulation == "dilate")
                seedImage$map(mmand::dilate, kernel=kernel)
            else if (boundaryManipulation == "inner")
                seedImage$map(function(x) x - mmand::erode(x,kernel=kernel))
            else if (boundaryManipulation == "outer")
                seedImage$map(function(x) mmand::dilate(x,kernel=kernel) - x)
        }
        
        seeds <- seedImage$find()
        report(outputLevel, "There are #{nrow(seeds)} candidate seeds for region #{index}")
        if (nrow(seeds) == 0)
            next
        
        if (strategy == "voxelwise")
        {
            labels <- apply(seeds, 1, implode, sep="_")
            for (i in seq_len(nrow(seeds)))
            {
                report(outputLevel-1, "Generating #{nStreamlines} streamlines from seed point (#{implode(seeds[i,],',')})")
                streamSource <- generateStreamlines(tracker, seeds[i,], nStreamlines, jitter=jitter)
                profiles[[labels[i]]] <- processStreamlines(streamSource, paste(tractName,labels[i],sep="_"))
            }
        }
        else
        {
            report(outputLevel, "Generating #{nStreamlines} streamlines from #{ifelse(randomSeeds,'random','specified')} seeds")
            label <- seedInfo$labels[which(seedInfo$indices == index)]
            if (randomSeeds)
            {
                seeds <- seeds[sample(nrow(seeds),nStreamlines,replace=TRUE),]
                streamSource <- generateStreamlines(tracker, seeds, 1L, jitter=jitter)
            }
            else
                streamSource <- generateStreamlines(tracker, seeds, nStreamlines, jitter=jitter)
            profiles[[label]] <- processStreamlines(streamSource, switch(strategy, regionwise=paste(tractName,label,sep="_"), tractName))
        }
    }
    
    endTime <- Sys.time()
    report(OL$Info, "Tracking completed in ", round(as.double(endTime-startTime,units="secs"),2), " seconds")
    
    if (requireProfile)
        write.csv(do.call(rbind,profiles), ensureFileSuffix(paste(tractName,"profile",sep="_"),"csv"))
}
