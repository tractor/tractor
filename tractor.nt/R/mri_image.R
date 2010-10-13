newMriImageAsVisitationMap <- function (streamSet, metadata = NULL)
{
    if (!isStreamlineSetTract(streamSet))
        output(OL$Error, "The specified tract is not a StreamlineSetTract object")
    if (is.null(metadata))
        metadata <- streamSet$getImageMetadata()
    else if (!isMriImageMetadata(metadata))
        output(OL$Error, "The specified metadata is not valid")
    
    if (streamSet$isOriginAtSeed())
        output(OL$Error, "Cannot create visitation maps for transformed streamline sets at the moment")
    
    dims <- metadata$getDimensions()
    
    nSamples <- streamSet$nStreamlines()
    leftPoints <- streamSet$getLeftPoints()
    rightPoints <- streamSet$getRightPoints()
    leftLengths <- streamSet$getLeftLengths()
    rightLengths <- streamSet$getRightLengths()
    
    data <- array(0, dim=dims)
    for (i in 1:nSamples)
    {
        currentPoints <- rbind(leftPoints[1:leftLengths[i],,i], rightPoints[1:rightLengths[i],,i])
        
        if (streamSet$getCoordinateUnit() == "mm")
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
