runNeighbourhoodTractography <- function (session, seed, refTract, faThreshold = 0.2, searchWidth = 7)
{
    if (!isFieldTract(refTract))
        output(OL$Error, "Reference tract must be specified as a FieldTract object")
    if (!is.numeric(seed) || !is.vector(seed) || (length(seed) != 3))
        output(OL$Error, "Central seed point must be specified as a numeric vector of length 3")
    
    output(OL$Info, "Creating reduced reference tract")
    reducedRefTract <- createReducedTractInfo(refTract)
    output(OL$Info, "Reference tract contains ", reducedRefTract$nVoxels, " nonzero voxels; length is ", reducedRefTract$length)
    
    searchNeighbourhood <- createNeighbourhoodInfo(searchWidth, centre=seed)
    candidateSeeds <- t(searchNeighbourhood$vectors)
    
    nSeeds <- searchWidth ^ 3
    middle <- (nSeeds %/% 2) + 1
    similarities <- rep(NA, searchWidth^3)
    
    output(OL$Info, "Starting neighbourhood tractography with FA threshold of ", faThreshold, " and search width of ", searchWidth)
    
    faImage <- session$getImageByType("fa")
    fas <- faImage[candidateSeeds]
    validSeeds <- c(middle, which(fas >= faThreshold))
    validSeeds <- validSeeds[!duplicated(validSeeds)]
    
    output(OL$Info, "Rejecting ", nSeeds-length(validSeeds), " seed points as below the FA threshold")
    
    runProbtrackWithSession(session, candidateSeeds[validSeeds,], requireFile=TRUE)
    for (d in validSeeds)
    {
        candidateTract <- newFieldTractFromProbtrack(session, candidateSeeds[d,], expectExists=TRUE, threshold=0.01)
        similarities[d] <- calculateSimilarity(reducedRefTract, candidateTract)
    }
    
    naiveSimilarity <- similarities[middle]
    bestLoc <- which.max(similarities)
    bestSimilarity <- similarities[bestLoc]
    bestSeed <- candidateSeeds[bestLoc,]
    
    output(OL$Info, "Naive similarity is ", round(naiveSimilarity,3), " at location ", implode(seed,","))
    output(OL$Info, "Best similarity is ", round(bestSimilarity,3), " at location ", implode(bestSeed,","))
    
    return (list(naiveSimilarity=naiveSimilarity, naiveSeed=seed, bestSimilarity=bestSimilarity, bestSeed=bestSeed))
}
