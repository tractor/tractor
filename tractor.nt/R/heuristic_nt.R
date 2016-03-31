runNeighbourhoodTractography <- function (session, seed, refTract, faThreshold = 0.2, searchWidth = 7, nStreamlines = 5000)
{
    if (!is(refTract, "FieldTract"))
        report(OL$Error, "Reference tract must be specified as a FieldTract object")
    if (!is.numeric(seed) || (length(seed) != 3))
        report(OL$Error, "Central seed point must be specified as a numeric vector of length 3")
    
    searchNeighbourhood <- createNeighbourhoodInfo(searchWidth, centre=seed)
    candidateSeeds <- t(searchNeighbourhood$vectors)
    
    nSeeds <- searchWidth ^ 3
    middle <- (nSeeds %/% 2) + 1
    similarities <- rep(NA, searchWidth^3)
    
    report(OL$Info, "Starting neighbourhood tractography with FA threshold of ", faThreshold, " and search width of ", searchWidth)
    
    faImage <- session$getImageByType("fa", "diffusion")
    fas <- faImage[candidateSeeds]
    validSeeds <- c(middle, which(fas >= faThreshold))
    validSeeds <- validSeeds[!duplicated(validSeeds)]
    report(OL$Info, "Rejecting ", nSeeds-length(validSeeds), " candidate seed points below the FA threshold")
    
    if (!equivalent(refTract$getVoxelDimensions(), faImage$getVoxelDimensions(), signMatters=FALSE, tolerance=1e-3))
    {
        report(OL$Info, "Resampling reference tract to the resolution of the session's native space")
        newRefImage <- tractor.reg::resampleImage(refTract$getImage(), faImage$getVoxelDimensions())
        newRefSeed <- round(tractor.reg::transformWorldToVoxel(tractor.reg::transformVoxelToWorld(refTract$getSeedPoint(), refTract$getImage(), simple=TRUE), newRefImage, simple=TRUE))
        refTract <- newFieldTractFromMriImage(newRefImage, newRefSeed)
    }
    
    report(OL$Info, "Creating reduced reference tract")
    reducedRefTract <- createReducedTractInfo(refTract)
    report(OL$Info, "Reference tract contains ", reducedRefTract$nVoxels, " nonzero voxels; length is ", reducedRefTract$length)
    
    tracker <- session$getTracker()
    for (i in seq_along(validSeeds))
    {
        currentSeed <- candidateSeeds[validSeeds[i],]
        report(OL$Verbose, "Current seed point is ", implode(currentSeed,sep=","), " (#{i}/#{length(validSeeds)})")
        
        trackerPath <- tracker$run(currentSeed, nStreamlines, requireMap=TRUE)
        imageForSeed <- readImageFile(trackerPath)
        candidateTract <- newFieldTractFromMriImage(imageForSeed, currentSeed)
        similarities[validSeeds[i]] <- calculateSimilarity(reducedRefTract, candidateTract)
    }
    
    naiveSimilarity <- similarities[middle]
    bestLoc <- which.max(similarities)
    bestSimilarity <- similarities[bestLoc]
    bestSeed <- candidateSeeds[bestLoc,]
    
    report(OL$Info, "Naive similarity is ", round(naiveSimilarity,3), " at location ", implode(seed,","))
    report(OL$Info, "Best similarity is ", round(bestSimilarity,3), " at location ", implode(bestSeed,","))
    
    return (list(naiveSimilarity=naiveSimilarity, naiveSeed=seed, bestSimilarity=bestSimilarity, bestSeed=bestSeed))
}
