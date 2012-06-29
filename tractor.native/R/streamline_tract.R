newStreamlineCollectionTractWithWaypointConstraints <- function (tract, waypoints, exclusion = FALSE)
{
    if (!is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineCollectionTract object")
    if (!is.list(waypoints))
        report(OL$Error, "Waypoints should be specified in a list")
    if (length(exclusion) != length(waypoints))
        exclusion <- rep(exclusion, length.out=length(waypoints))
    
    maskPoints <- lapply(waypoints, function (image) {
        if (!is(image, "MriImage"))
            report(OL$Error, "Waypoint images must be specified as MriImage objects")
        if (image$getDimensionality() != 3)
            report(OL$Error, "Waypoint images must be three-dimensional")
        return (which(image$getData() > 0, arr.ind=TRUE))
    })
    nMaskPoints <- sapply(maskPoints, nrow)
    
    lengths <- tract$getEndIndices() - tract$getStartIndices() + 1
    matchingIndices <- .Call("find_waypoint_hits", as.integer(round(tract$getPoints())), as.integer(tract$nPoints()), as.integer(tract$getStartIndices()), as.integer(lengths), as.integer(tract$nStreamlines()), maskPoints, as.integer(length(maskPoints)), as.integer(nMaskPoints), as.integer(exclusion), PACKAGE="tractor.native")
    
    report(OL$Info, length(matchingIndices), " of ", tract$nStreamlines(), " streamlines meet the specified waypoint constraint(s)")
    
    if (length(matchingIndices) > 0)
    {
        newTract <- newStreamlineCollectionTractBySubsetting(tract, matchingIndices)
        invisible(newTract)
    }
    else
        invisible(NULL)
}
