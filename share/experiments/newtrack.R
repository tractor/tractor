#@args session directory, [seed region(s)]

library(tractor.track)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    nStreamlines <- getConfigVariable("Streamlines", 5000L, "integer")
    blockType <- getConfigVariable("BlockType", "image", validValues=c("image","region","seed"))
    boundaryManipulation <- getConfigVariable("BoundaryManipulation", "none", validValues=c("none","erode","dilate","inner","outer"))
    kernelShape <- getConfigVariable("KernelShape", "diamond", "character", validValues=c("box","disc","diamond"))
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL, "numeric")
    jitter <- getConfigVariable("JitterSeeds", FALSE)
    
    seedRegions <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    wholeBrainSeeding <- (length(seedRegions) == 0)
    sequentialSeeding <- (blockType == "seed")
    
    mask <- session$getImageByType("mask", "diffusion", metadataOnly=!wholeBrainSeeding)
    seedImage <- mask$copy()
    
    if (wholeBrainSeeding)
        indices <- 1L
    else
    {
        indices <- NULL
        seedImage$fill(0L)
        areFiles <- imageFileExists(seedRegions)
        
        if (any(!areFiles))
        {
            parcellation <- session$getParcellation("diffusion")
            indices <- matchRegions(seedRegions[!areFiles], parcellation)
            locs <- which(parcellation$image$getData() %in% indices, arr.ind=TRUE)
            seedImage[locs] <- parcellation$image[locs]
        }
        
        for (region in seedRegions[areFiles])
        {
            # This makes "data" a SparseArray object
            data <- readImageFile(region,sparse=TRUE)$getData()
            positive <- (data$getData() > 0)
            locs <- data$getCoordinates()[positive,]
            currentIndices <- unique(data$getData()[positive])
            
            if (!all(currentIndices == round(currentIndices)))
                report(OL$Error, "Seed image must be integer-valued")
            
            if (any(currentIndices %in% indices))
                delta <- max(indices)
            else
                delta <- 0L
            
            seedImage[locs] <- data[locs] + delta
            indices <- c(indices, as.integer(currentIndices + delta))
        }
    }
    
    if (!is.null(anisotropyThreshold))
    {
        fa <- session$getImageByType("FA")
        subthreshold <- which(fa$getData() < anisotropyThreshold, arr.ind=TRUE)
        seedImage[subthreshold] <- 0L
    }
    
    if (blockType == "image")
    {
        indices <- 1L
        seedImage[seedImage$getNonzeroIndices()] <- 1L
    }
}
