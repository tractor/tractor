#@desc Run tractography for a session containing diffusion data, either for the entire seed area at once (Strategy:global) or regionwise or voxelwise. The number of streamlines generated in each case may be given as a literal integer (in which case points are chosen randomly for each streamline) or as an integer followed by "x", in which case that many will be generated for each eligible seed. Seed regions may be voxel locations (given using the R voxel convention), image file names or named regions in a parcellation. If RequirePaths:true is given then streamlines will be saved in TrackVis .trk format. If target regions are also specified then an auxiliary label file with extension .trkl is also created, which maps streamlines onto the targets they reached.
#@args session directory, [seed region(s)]
#@example # Seed everywhere within the brain mask
#@example tractor track /data/subject1
#@example # Seed in all white matter and use cortical grey matter regions as targets
#@example tractor track /data/subject1 white_matter TargetRegions:cerebal_cortex TerminateAtTargets:true
#@example # Seed just outside a particular grey matter region
#@example tractor track /data/subject1 postcentral_gyrus_left BoundaryManipulation:outer

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
    wholeBrainSeeding <- (length(seedRegions) == 0)
    if (!is.null(targetRegions))
        targetRegions <- splitAndConvertString(targetRegions, ",", fixed=TRUE)
    
    mask <- session$getImageByType("mask", "diffusion")
    
    applyBoundaryManipulation <- function (image)
    {
        if (boundaryManipulation != "none")
        {
            kernel <- mmand::shapeKernel(c(3,3,3), type=kernelShape)
            if (boundaryManipulation == "erode")
                image$map(mmand::erode, kernel=kernel)
            else if (boundaryManipulation == "dilate")
                image$map(mmand::dilate, kernel=kernel)
            else if (boundaryManipulation == "inner")
                image$map(function(x) x - mmand::erode(x,kernel=kernel))
            else if (boundaryManipulation == "outer")
                image$map(function(x) mmand::dilate(x,kernel=kernel) - x)
        }
        
        return (image)
    }
    
    if (wholeBrainSeeding)
        seedInfo <- list(image=mask$copy(), indices=1L, labels="brain")
    else if (length(seedRegions) %% 3 == 0 && isValidAs(seedRegions,"integer"))
    {
        seedMatrix <- matrix(as.integer(seedRegions), ncol=3, byrow=TRUE)
        seedImage <- mask$copy()$fill(0L)
        seedImage[seedMatrix] <- 1L
        seedInfo <- list(image=seedImage, indices=1L, labels="points")
    }
    else
        seedInfo <- resolveRegions(seedRegions, session, "diffusion", parcellationConfidence)
    
    if (is.null(anisotropyThreshold))
        seedInfo$image <- seedInfo$image * mask
    else
    {
        fa <- session$getImageByType("FA")
        subthreshold <- which(fa$getData() < anisotropyThreshold, arr.ind=TRUE)
        seedInfo$image[subthreshold] <- 0L
    }
    
    if (!is.null(targetRegions))
        targetInfo <- resolveRegions(targetRegions, session, "diffusion", parcellationConfidence)
    else
        targetInfo <- list(image=NULL, indices=NULL, labels=NULL)
    
    if (requireProfile && length(targetInfo$indices) > 0)
    {
        profile <- matrix(NA, nrow=0, ncol=length(targetInfo$indices), dimnames=list(NULL,targetInfo$labels))
        profileFun <- function (indices, counts)
        {
            line <- rep(NA, length(targetInfo$indices))
            line[match(indices,targetInfo$indices)] <- counts
            profile <<- rbind(profile, line)
        }
    }
    else
        profileFun <- NULL
    
    if (minTargetHits == "all")
        minTargetHits <- length(targetInfo$indices)
    else if (!isValidAs(minTargetHits, "integer"))
        report(OL$Error, "MinTargetHits must be an integer or \"all\"")
    else
        minTargetHits <- as.integer(minTargetHits)
    
    tracker <- session$getTracker(mask, preferredModel=preferredModel)
    tracker$setTargets(targetInfo)
    tracker$setOptions(stepLength=stepLength)
    tracker$setFilters(minLength=minLength, maxLength=maxLength, minTargetHits=minTargetHits)
    report(OL$Info, "Using #{toupper(tracker$getModel()$getType())} diffusion model")
    
    if (strategy == "global")
    {
        seedImage <- applyBoundaryManipulation(seedInfo$image$copy()$binarise())
        seeds <- seedImage$getNonzeroIndices(array=TRUE)
        if (randomSeeds && nrow(seeds) > 1)
        {
            report(OL$Info, "Performing global tractography to generate #{nStreamlines} streamlines from #{nrow(seeds)} candidate seeds")
            seeds <- seeds[sample(nrow(seeds),nStreamlines,replace=TRUE),]
            tracker$run(seeds, count=1L, tractName, profileFun=profileFun, requireMap=requireMap, requireStreamlines=requireStreamlines, terminateAtTargets=terminateAtTargets, jitter=jitter)
        }
        else
        {
            report(OL$Info, "Performing sequential global tractography with #{nrow(seeds)} seed(s), #{nStreamlines} streamlines per seed")
            tracker$run(seeds, count=nStreamlines, tractName, profileFun=profileFun, requireMap=requireMap, requireStreamlines=requireStreamlines, terminateAtTargets=terminateAtTargets, jitter=jitter)
        }
        
        if (!is.null(profileFun))
            rownames(profile) <- tractName
    }
    else if (strategy == "regionwise")
    {
        report(OL$Info, "Performing regionwise tractography for #{length(seedInfo$indices)} distinct regions")
        for (index in seedInfo$indices)
        {
            label <- seedInfo$labels[which(seedInfo$indices == index)]
            seedImage <- applyBoundaryManipulation(seedInfo$image$copy()$map(function(x) ifelse(x==index,1L,0L)))
            seeds <- seedImage$getNonzeroIndices(array=TRUE)
            if (randomSeeds && nrow(seeds) > 1)
            {
                report(OL$Verbose, "Generating #{nStreamlines} streamlines from #{nrow(seeds)} candidate seeds in region #{label}")
                seeds <- seeds[sample(nrow(seeds),nStreamlines,replace=TRUE),]
                tracker$run(seeds, count=1L, paste(tractName,label,sep="_"), profileFun=profileFun, requireMap=requireMap, requireStreamlines=requireStreamlines, terminateAtTargets=terminateAtTargets, jitter=jitter)
            }
            else
            {
                report(OL$Verbose, "Tracking in region #{label} with #{nrow(seeds)} seed(s), #{nStreamlines} streamlines per seed")
                tracker$run(seeds, count=nStreamlines, paste(tractName,label,sep="_"), profileFun=profileFun, requireMap=requireMap, requireStreamlines=requireStreamlines, terminateAtTargets=terminateAtTargets, jitter=jitter)
            }
        }
        
        if (!is.null(profileFun))
            rownames(profile) <- seedInfo$labels
    }
    else if (strategy == "voxelwise")
    {
        seedImage <- applyBoundaryManipulation(seedInfo$image$copy()$binarise())
        seeds <- seedImage$getNonzeroIndices(array=TRUE)
        
        report(OL$Info, "Performing voxelwise tractography for #{nrow(seeds)} distinct seed points")
        labels <- apply(seeds, 1, implode, sep="_")
        for (i in seq_len(nrow(seeds)))
        {
            report(OL$Verbose, "Generating #{nStreamlines} streamlines from seed point (#{implode(seeds[i,],',')})")
            tracker$run(seeds[i,], count=nStreamlines, paste(tractName,labels[i],sep="_"), profileFun=profileFun, requireMap=requireMap, requireStreamlines=requireStreamlines, terminateAtTargets=terminateAtTargets, jitter=jitter)
        }
        
        if (!is.null(profileFun))
            rownames(profile) <- labels
    }
    
    if (!is.null(profileFun))
        write.csv(profile, ensureFileSuffix(paste(tractName,"profile",sep="_"),"csv"))
}
