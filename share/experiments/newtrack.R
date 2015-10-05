#@args session directory, [seed region(s)]
#@example tractor track /data/subject1

library(ore)
library(tractor.track)
library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    strategy <- getConfigVariable("Strategy", "global", validValues=c("global","regionwise","voxelwise"))
    nStreamlines <- getConfigVariable("Streamlines", "100x")
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL, "numeric")
    boundaryManipulation <- getConfigVariable("BoundaryManipulation", "none", validValues=c("none","erode","dilate","inner","outer"))
    kernelShape <- getConfigVariable("KernelShape", "diamond", "character", validValues=c("box","disc","diamond"))
    jitter <- getConfigVariable("JitterSeeds", FALSE)
    targetRegions <- getConfigVariable("TargetRegions", NULL, "character")
    terminateAtTargets <- getConfigVariable("TerminateAtTargets", FALSE)
    minTargetHits <- getConfigVariable("MinTargetHits", "0", "character")
    minLength <- getConfigVariable("MinLength", 0)
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
                image <- newMriImageWithSimpleFunction(image, mmand::erode, kernel=kernel)
            else if (boundaryManipulation == "dilate")
                image <- newMriImageWithSimpleFunction(image, mmand::dilate, kernel=kernel)
            else if (boundaryManipulation == "inner")
                image <- newMriImageWithSimpleFunction(image, function(x) x - mmand::erode(x,kernel=kernel))
            else if (boundaryManipulation == "outer")
                image <- newMriImageWithSimpleFunction(image, function(x) mmand::dilate(x,kernel=kernel) - x)
        }
        
        return (image)
    }
    
    mergeRegions <- function (regionNames)
    {
        indices <- labels <- NULL
        image <- mask$copy()$fill(0L)
        areFiles <- imageFileExists(regionNames)
        
        if (any(!areFiles))
        {
            parcellation <- session$getParcellation("diffusion")
            indices <- sort(matchRegions(regionNames[!areFiles], parcellation))
            labels <- parcellation$regions$label[parcellation$regions$index %in% indices]
            locs <- which(parcellation$image$getData() %in% indices, arr.ind=TRUE)
            image[locs] <- parcellation$image[locs]
        }
        
        for (region in regionNames[areFiles])
        {
            # This makes "data" a SparseArray object
            currentImage <- readImageFile(region, sparse=TRUE)
            data <- currentImage$getData()
            positive <- (data$getData() > 0)
            locs <- data$getCoordinates()[positive,]
            currentIndices <- sort(unique(data$getData()[positive]))
            
            if (length(currentIndices) == 0)
                next
            
            if (!all(currentIndices == round(currentIndices)))
                report(OL$Error, "ROI image must be integer-valued")
            
            if (any(currentIndices %in% indices))
                delta <- max(indices)
            else
                delta <- 0L
            
            image[locs] <- data[locs] + delta
            indices <- c(indices, as.integer(currentIndices + delta))
            
            if (length(currentIndices) == 1)
                labels <- c(labels, basename(currentImage$getSource()))
            else
                labels <- c(labels, paste(basename(currentImage$getSource()),currentIndices,sep="_"))
        }
        
        return (list(image=image, indices=indices, labels=labels))
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
        seedInfo <- mergeRegions(seedRegions)
    
    if (is.null(anisotropyThreshold))
        seedInfo$image <- seedInfo$image * mask
    else
    {
        fa <- session$getImageByType("FA")
        subthreshold <- which(fa$getData() < anisotropyThreshold, arr.ind=TRUE)
        seedInfo$image[subthreshold] <- 0L
    }
    
    if (!is.null(targetRegions))
        targetInfo <- mergeRegions(targetRegions)
    else
        targetInfo <- list(image=NULL, indices=integer(0), labels=character(0))
    
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
    
    tracker <- session$getTracker(mask, targetInfo$image)
    tracker$setFilters(minLength=minLength, minTargetHits=minTargetHits)
    
    if (strategy == "global")
    {
        seedImage <- applyBoundaryManipulation(newMriImageWithSimpleFunction(seedInfo$image, function(x) ifelse(x>0,1L,0L)))
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
            seedImage <- applyBoundaryManipulation(newMriImageWithSimpleFunction(seedInfo$image, function(x) ifelse(x==index,1L,0L)))
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
        seedImage <- applyBoundaryManipulation(newMriImageWithSimpleFunction(seedInfo$image, function(x) ifelse(x>0,1L,0L)))
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
