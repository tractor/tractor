#@args session directory
#@desc Run tractography for connectivity "profiling", generating a table of streamline counts connecting each voxel in the seed region to each target region. Only the internal tracker can currently be used for this purpose. For generating tract images, use "rtrack" instead; for regionwise tracking use "mtrack".

library(tractor.session)
library(tractor.nt)
library(tractor.native)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    seedMaskFile <- getConfigVariable("SeedMaskFile", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    seedMaskInStandardSpace <- getConfigVariable("SeedMaskInStandardSpace", FALSE)
    targetMaskFiles <- getConfigVariable("TargetMaskFiles", NULL, "character", errorIfInvalid=TRUE, errorIfMissing=TRUE)
    targetMasksInStandardSpace <- getConfigVariable("TargetMasksInStandardSpace", FALSE)
    targetNames <- getConfigVariable("TargetNames", NULL)
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    anisotropyThreshold <- getConfigVariable("AnisotropyThreshold", NULL)
    
    seedMask <- newMriImageFromFile(seedMaskFile)
    if (seedMaskInStandardSpace)
        seedMask <- transformStandardSpaceImage(session, seedMask)
    
    if (!is.null(anisotropyThreshold))
    {
        faImage <- session$getImageByType("FA", "diffusion")
        seedMask <- newMriImageWithBinaryFunction(seedMask, faImage, function (x,y) replace(x, y < anisotropyThreshold, 0))
    }
    
    targetMaskFiles <- splitAndConvertString(as.character(targetMaskFiles), ",", fixed=TRUE)
    targetMasks <- list()
    for (targetFile in targetMaskFiles)
    {
        targetMask <- newMriImageFromFile(targetFile)
        if (targetMasksInStandardSpace)
            targetMask <- transformStandardSpaceImage(session, targetMask)
        targetMasks <- c(targetMasks, list(targetMask))
    }
    
    if (is.null(targetNames))
        targetNames <- ensureFileSuffix(basename(targetMaskFiles))
    
    seeds <- which(seedMask$getData() > 0, arr.ind=TRUE)
    if (nrow(seeds) == 0)
        report(OL$Error, "There are no seed points")
    
    streamlineCounts <- matrix(NA, nrow=nrow(seeds), ncol=length(targetNames), dimnames=list(NULL,targetNames))
    for (i in 1:nrow(seeds))
    {
        result <- trackWithSession(session, seeds[i,], nSamples=nSamples, requireImage=FALSE, requireStreamlines=TRUE)
        for (j in 1:length(targetNames))
            streamlineCounts[i,j] <- length(findWaypointHits(result$streamlines, targetMasks[j]))
    }
    
    data <- cbind(seeds, streamlineCounts)
    colnames(data)[1:3] <- c("X", "Y", "Z")
    write.table(data, paste(basename(seedMask$getSource()), "profile.txt", sep="_"))
}
