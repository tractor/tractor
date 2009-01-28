findVoxelValues <- function (tract, centre, unvisited, searchNeighbourhood)
{
    if (!isNeighbourhoodInfo(searchNeighbourhood))
        output(OL$Error, "Specified search neighbourhood is not a NeighbourhoodInfo object")
    
    dims <- tract$getDimensions()
    if (!identical(dim(unvisited), dims))
        output(OL$Error, "The unvisited array must have the same dimensions as the tract data")
    
    indices <- searchNeighbourhood$vectors + centre
    indices <- replace(indices, which(indices < 1 | indices > dims), NA)
    indices <- t(indices)
    
    voxelValues <- tract$getData()[indices] * unvisited[indices]
    return (voxelValues)
}

calculateMatchingScore <- function (refTract, candTract, trueLengths = FALSE, biasReferenceSteps = FALSE, maxReps = NA, requireRoute = FALSE)
{
    chooseVector <- function (voxelValues, searchNeighbourhood, previous = NA)
    {
        # In the event of a tie, we favour continuity
        maxLocs <- which(max(voxelValues,na.rm=TRUE) == voxelValues)
        if (length(maxLocs) > 1 && !is.na(previous))
        {
            innerProducts <- searchNeighbourhood$innerProducts[previous,maxLocs]
            smallestAngles <- which(max(innerProducts) == innerProducts)
            maxLocs <- maxLocs[smallestAngles]
            
            # We shouldn't give this warning when we're at the seed point (as
            # when 'previous' is NA), since in that case all voxels will be
            # visited eventually anyway
            if (length(maxLocs) > 1)
                flag(OL$Warning, "Maximal voxel value is not unique")
        }
        
        if (length(maxLocs) > 1)
            return (sample(maxLocs, 1))
        else
            return (maxLocs)
    }
    
    if (!isFieldTract(refTract) || !isFieldTract(candTract))
        output(OL$Error, "Reference and candidate tracts must be specified as FieldTract objects")
    if (!equivalent(refTract$getVoxelDimensions(), candTract$getVoxelDimensions(), signMatters=FALSE, tolerance=1e-3))
        output(OL$Error, "Reference and candidate tracts must have the same voxel dimensions")
    
    refDims <- refTract$getDimensions()
    candDims <- candTract$getDimensions()
    refUnvisited <- array(TRUE, dim=refDims)
    candUnvisited <- array(TRUE, dim=candDims)
    
    searchNeighbourhood <- createNeighbourhoodInfo(3)
    distanceVectors <- searchNeighbourhood$vectors * refTract$getVoxelDimensions()
    distances <- sqrt(colSums(distanceVectors^2))
    
    score <- 0
    length <- 0
    reps <- 0
    
    refFirstLoc <- NA
    route <- matrix(0, nrow=1, ncol=3)
    
    repeat
    {
        if (isTRUE(reps == maxReps))
            break
        
        starting <- TRUE
        refPreviousLoc <- NA
        refMaxLoc <- NA
        refPointer <- refTract$getSeedPoint()
        candPointer <- candTract$getSeedPoint()
        
        repeat
        {
            route <- rbind(route, candPointer, deparse.level=0)
            refUnvisited[matrix(refPointer,nrow=1)] <- FALSE
            candUnvisited[matrix(candPointer,nrow=1)] <- FALSE
            
            refVoxelValues <- findVoxelValues(refTract, refPointer, refUnvisited, searchNeighbourhood)
            if (biasReferenceSteps)
            {
                if (!is.na(refMaxLoc))
                {
                    ip <- searchNeighbourhood$innerProducts[refMaxLoc,]
                    refVoxelValues <- refVoxelValues * sqrt(ifelse(ip <= 0, 0, ip))
                }
                else if (!is.na(refFirstLoc) && (reps == 1))
                    refVoxelValues <- refVoxelValues * -sign(searchNeighbourhood$innerProducts[refFirstLoc,])
            }
            if (max(refVoxelValues,na.rm=TRUE) <= 0)
                break
            
            refMaxLoc <- chooseVector(refVoxelValues, searchNeighbourhood, refPreviousLoc)
            
            if (is.na(refFirstLoc))
                refFirstLoc <- refMaxLoc
            refPreviousLoc <- refMaxLoc
            
            candVoxelValues <- findVoxelValues(candTract, candPointer, candUnvisited, searchNeighbourhood)
            candVoxelValues <- candVoxelValues * sign(searchNeighbourhood$innerProducts[refMaxLoc,])
            if (max(candVoxelValues,na.rm=TRUE) <= 0)
                break
            
            candMaxLoc <- chooseVector(candVoxelValues, searchNeighbourhood, refMaxLoc)
            
            scoreContribution <- searchNeighbourhood$innerProducts[refMaxLoc,candMaxLoc]
            score <- score + scoreContribution
            
            candPointer <- candPointer + searchNeighbourhood$vectors[,candMaxLoc]
            refPointer <- refPointer + searchNeighbourhood$vectors[,refMaxLoc]
            
            if (trueLengths)
                length <- length + distances[refMaxLoc]
            else
                length <- length + 1
            
            starting <- FALSE
        }
        
        if (starting)
            break
        else
            reps <- reps + 1
    }
    
    returnValue <- list(score=score, length=length, refMask=(!refUnvisited), reps=reps)
    if (requireRoute)
        returnValue <- c(returnValue, list(route=route[-1,]))
    
    return (returnValue)
}

createReducedTractInfo <- function (tract, biasSteps = FALSE, maxReps = NA)
{
    match <- calculateMatchingScore(tract, tract, biasReferenceSteps=biasSteps, maxReps=maxReps)
    reducedTract <- newFieldTractByMasking(tract, match$refMask)
    result <- list(tract=reducedTract, nVoxels=match$score, length=match$length)
    class(result) <- c("info.tract.reduced", "list")
    invisible (result)
}

isReducedTractInfo <- function (object)
{
    return ("info.tract.reduced" %in% class(object))
}

calculateSimilarity <- function (reference, candidate)
{
    if (!isReducedTractInfo(reference))
        reference <- createReducedTractInfo(reference)
    if (!isReducedTractInfo(candidate))
        candidate <- createReducedTractInfo(candidate)
    
    match <- calculateMatchingScore(reference$tract, candidate$tract)
    
    shapeSimilarity <- match$score / min(reference$nVoxels, candidate$nVoxels)
    lengthSimilarity <- 1 - abs((reference$length - candidate$length)/(reference$length + candidate$length))
    
    if (!is.finite(shapeSimilarity))
        shapeSimilarity <- 0
    if (!is.finite(lengthSimilarity))
        lengthSimilarity <- 0
    if ((shapeSimilarity > 1) || (lengthSimilarity > 1))
        output(OL$Warning, "Score component greater than one (", max(shapeSimilarity,lengthSimilarity), ")")
    if ((shapeSimilarity < 0) || (lengthSimilarity < 0))
        output(OL$Warning, "Score component less than zero (", min(shapeSimilarity,lengthSimilarity), ")")
    
    similarity <- sqrt(shapeSimilarity * lengthSimilarity)
    return (similarity)
}
