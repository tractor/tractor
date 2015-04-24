#@args session directory, [seed region(s)]
#@example tractor track /data/subject1

library(ore)
library(tractor.track)
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
            indices <- sort(matchRegions(seedRegions[!areFiles], parcellation))
            labels <- parcellation$regions$label[parcellation$regions$index %in% indices]
            locs <- which(parcellation$image$getData() %in% indices, arr.ind=TRUE)
            image[locs] <- parcellation$image[locs]
        }
        
        for (region in seedRegions[areFiles])
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
            
            seedImage[locs] <- data[locs] + delta
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
    
    diffusionModel <- bedpostDiffusionModel(session$getDirectory("bedpost"))
    
    if (strategy == "global")
    {
        seedImage <- applyBoundaryManipulation(newMriImageWithSimpleFunction(seedInfo$image, function(x) ifelse(x>0,1L,0L)))
        seeds <- seedImage$getNonzeroIndices(array=TRUE)
        if (randomSeeds)
        {
            report(OL$Info, "Performing global tractography to generate #{nStreamlines} streamlines from #{nrow(seeds)} candidate seeds")
            seeds <- seeds[sample(nrow(seeds),nStreamlines,replace=TRUE),]
            diffusionModel$track(seeds, count=1L, mask, tractName, requireMap=requireMap, requireStreamlines=requireStreamlines, requireProfile=requireProfile, jitter=jitter)
        }
        else
        {
            report(OL$Info, "Performing sequential global tractography with #{nrow(seeds)} seeds, #{nStreamlines} streamlines per seed")
            diffusionModel$track(seeds, count=nStreamlines, mask, tractName, requireMap=requireMap, requireStreamlines=requireStreamlines, requireProfile=requireProfile, jitter=jitter)
        }
    }
}
