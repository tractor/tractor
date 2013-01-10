newMriImageAsVisitationMap <- function (tract, metadata = NULL)
{
    if (!is(tract, "StreamlineSetTract") && !is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineSetTract or StreamlineCollectionTract object")
    if (is.null(metadata))
        metadata <- tract$getImageMetadata()
    else if (!is(metadata, "MriImage"))
        report(OL$Error, "The specified metadata is not valid")
    
    if (tract$isOriginAtSeed())
        report(OL$Error, "Cannot create visitation maps for transformed streamline sets at the moment")
    
    dims <- metadata$getDimensions()
    
    nSamples <- tract$nStreamlines()
    if (is(tract, "StreamlineSetTract"))
    {
        leftPoints <- tract$getLeftPoints()
        rightPoints <- tract$getRightPoints()
        leftLengths <- tract$getLeftLengths()
        rightLengths <- tract$getRightLengths()
    }
    
    data <- array(0, dim=dims)
    for (i in 1:nSamples)
    {
        if (is(tract, "StreamlineSetTract"))
            currentPoints <- rbind(leftPoints[1:leftLengths[i],,i], rightPoints[1:rightLengths[i],,i])
        else
            currentPoints <- tract$getPoints(i)
        
        if (tract$getCoordinateUnit() == "mm")
            currentPoints <- transformWorldToRVoxel(currentPoints, metadata)
        currentPoints <- round(currentPoints)
        
        # ProbTrack allows tracking to voxel positions in [0.5, dims+0.5]
        # Rounding points that fall exactly on these extrema can lead to
        # out-of-bounds indices, since e.g. round(0.5)==0
        # We therefore have to find and remove points at zero and dims+1
        outOfBounds <- as.logical(colSums(apply(currentPoints,1,">",dims) + as.integer(currentPoints==0)))
        currentPoints <- currentPoints[!outOfBounds,]
        
        data[currentPoints] <- data[currentPoints] + 1
    }
    
    image <- newMriImageWithData(data, metadata)
    invisible (image)
}
