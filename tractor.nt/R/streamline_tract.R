.StreamlineTractMetadata <- function (.isOriginAtSeed, .coordUnit, .imageMetadata)
{
    if (!(.coordUnit %in% c("vox","mm")))
        output(OL$Error, "Coordinate unit must be 'vox' (for voxels) or 'mm' (for millimetres)")
    if (!isMriImageMetadata(.imageMetadata))
        output(OL$Error, "Specified image metadata is not an MriImageMetadata object")
    
    self <- list(
        getCoordinateUnit = function () { return (.coordUnit) },
        
        getImageMetadata = function () { return (.imageMetadata) },
        
        getSpatialRange = function () { output(OL$Error, "Method 'getSpatialRange' undefined") },
        
        isOriginAtSeed = function () { return (.isOriginAtSeed) }
    )
    
    class(self) <- c("metadata.tract.streamline", "list.object", "list")
    self <- inherit(self, .imageMetadata)
    invisible (self)
}

.StreamlineTract <- function (.line, .seedIndex, .originalSeedPoint, .metadata)
{
    if (!is.matrix(.line) || (dim(.line)[2] != 3))
        output(OL$Error, "Streamline must be specified as a matrix with 3 columns")
    if (!isStreamlineTractMetadata(.metadata))
        output(OL$Error, "Specified metadata is not valid")
    
    .pointSpacings <- apply(diff(.line), 1, vectorLength)
    
    self <- list(
        getLine = function () { return (.line) },
        
        getLineLength = function () { return (sum(.pointSpacings)) },
        
        getMetadata = function () { return (.metadata) },
        
        getPointSpacings = function () { return (.pointSpacings) },
        
        getSeedIndex = function () { return (.seedIndex) },
        
        getOriginalSeedPoint = function () { return (.originalSeedPoint) },
        
        getSeedPoint = function () { return (.originalSeedPoint) },
        
        getSpatialRange = function ()
        {
            fullRange <- apply(.line, 2, range, na.rm=TRUE)
            return (list(mins=fullRange[1,], maxes=fullRange[2,]))
        },
        
        nPoints = function () { return (dim(.line)[1]) }
    )
    
    class(self) <- c("tract.streamline", "list.object", "list")
    self <- inherit(self, .metadata)
    invisible (self)
}

.StreamlineSetTract <- function (.seedPoint, .leftLengths, .rightLengths, .leftPoints, .rightPoints, .metadata)
{
    if (!is.numeric(.seedPoint) || (length(.seedPoint) != 3))
        output(OL$Error, "Seed point should be specified as a 3-vector")
    if (!isStreamlineTractMetadata(.metadata))
        output(OL$Error, "Specified metadata is not valid")
    
    self <- list(
        getLeftLengths = function () { return (.leftLengths) },
        
        getLeftPoints = function () { return (.leftPoints) },
        
        getMetadata = function () { return (.metadata) },
        
        getRightLengths = function () { return (.rightLengths) },
        
        getRightPoints = function () { return (.rightPoints) },
        
        getSeedPoint = function () { return (.seedPoint) },
        
        getSpatialRange = function ()
        {
            leftRange <- apply(.leftPoints, 2, range, na.rm=TRUE)
            rightRange <- apply(.rightPoints, 2, range, na.rm=TRUE)
            mins <- pmin(leftRange,rightRange)[1,]
            maxes <- pmax(leftRange,rightRange)[2,]
            return (list(mins=mins, maxes=maxes))
        },
        
        nStreamlines = function () { return (dim(.leftPoints)[3]) }
    )
    
    class(self) <- c("tract.streamline.set", "list.object", "list")
    self <- inherit(self, .metadata)
    invisible (self)
}

isStreamlineTractMetadata <- function (object)
{
    return ("metadata.tract.streamline" %in% class(object))
}

isStreamlineTract <- function (object)
{
    return ("tract.streamline" %in% class(object))
}

isStreamlineSetTract <- function (object)
{
    return ("tract.streamline.set" %in% class(object))
}

newStreamlineTractMetadataFromImageMetadata <- function (imageMetadata, originAtSeed, coordUnit)
{
    tractMetadata <- .StreamlineTractMetadata(originAtSeed, coordUnit, imageMetadata)
    invisible (tractMetadata)
}

newStreamlineSetTractFromProbtrack <- function (session, x, y = NULL, z = NULL, nSamples = 5000, maxPathLength = NULL)
{
    probtrackResult <- runProbtrackWithSession(session, x, y, z, requireParticlesDir=TRUE, nSamples=nSamples, verbose=TRUE, force=TRUE)
    
    seed <- resolveVector(len=3, x, y, z)
    axisNames <- c("left-right", "anterior-posterior", "inferior-superior")
    
    if (is.null(maxPathLength))
    {
        fileSizes <- particleFileSizesForResult(probtrackResult)
        maxPathLength <- round(max(fileSizes) / 20)
        output(OL$Info, "Setting maximum path length to ", maxPathLength)
    }
    
    # In order to estimate appropriate parameters for the calculations and
    # plots below, we initially read in a subsample of the streamlines
    subSize <- ifelse(nSamples < 40, nSamples, ceiling(nSamples / 20))
    subData <- array(NA, dim=c(maxPathLength,3,subSize))
    output(OL$Info, "Reading subsample of ", subSize, " streamlines")
    
    for (i in 1:subSize)
    {
        sampleData <- retrieveProbtrackStreamline(probtrackResult, i)
        len <- length(sampleData[,1])
        
        if (len > maxPathLength)
        {
            output(OL$Warning, "Path length for streamline ", i, " is too long (", len, "); truncating")
            len <- maxPathLength
        }
        
        subData[1:len,,i] <- sampleData[1:len,]
    }
    output(OL$Info, "Done; analyzing subsample")
    
    # Whichever dimension changes most in the first step from the seed point
    # in the subsample will be used to split "left" from "right"
    firstSteps <- subData[2,,] - subData[1,,]
    meanFirstStep <- apply(abs(firstSteps), 1, mean)
    testAxis <- which.max(meanFirstStep)
    output(OL$Info, "Using ", axisNames[testAxis], " axis for left/right splitting")
    
    # Remove the subsample to save memory
    rm(subData)
    
    leftLengths <- numeric(0)
    rightLengths <- numeric(0)
    leftData <- array(NA, dim=c(maxPathLength,3,nSamples))
    rightData <- array(NA, dim=c(maxPathLength,3,nSamples))
    output(OL$Info, "Reading data from ", nSamples, " streamlines...")
    
    # Read all the data in, separating out "left" and "right" streamline parts
    for (i in 1:nSamples)
    {       
        sampleData <- retrieveProbtrackStreamline(probtrackResult, i)
        len <- length(sampleData[,1])
        
        # If any streamline is too long, we have to truncate it
        if (len > maxPathLength)
        {
            output(OL$Warning, "Path length for streamline ", i, " (", len, ") is too long; truncating")
            len <- maxPathLength
        }
        
        startRows <- which(sampleData[,1]==seed[1] & sampleData[,2]==seed[2] & sampleData[,3]==seed[3])
        
        # The seed point should appear exactly twice in each file, but it
        # occasionally appears twice in succession for some unknown reason
        # The following "paragraph" is a hack to fix this problem
        repeats <- which(diff(startRows) == 1) + 1
        for (rep in repeats)
        {
            sampleData <- sampleData[-startRows[rep],]
            startRows <- startRows[-rep]
            len <- len - 1
        }
        
        if (length(startRows) != 2)
            output(OL$Error, "Seed point appears ", length(startRows), " times (expecting 2) in streamline ", i)
        restartRow <- startRows[2]
        if (restartRow > len)
            output(OL$Error, "Left and right line components do not appear within the specified maximum path length")
        
        # This left/right distinction is a little flakey...
        if (sampleData[2,testAxis] < sampleData[1,testAxis])
        {
            leftLengths <- c(leftLengths, (restartRow-1))
            rightLengths <- c(rightLengths, (len-restartRow+1))
            leftData[1:(restartRow-1),,i] <- sampleData[1:(restartRow-1),]
            rightData[1:(len-restartRow+1),,i] <- sampleData[restartRow:len,]
        }
        else
        {
            rightLengths <- c(rightLengths, (restartRow-1))
            leftLengths <- c(leftLengths, (len-restartRow+1))
            rightData[1:(restartRow-1),,i] <- sampleData[1:(restartRow-1),]
            leftData[1:(len-restartRow+1),,i] <- sampleData[restartRow:len,]
        }
        
        if (i %% subSize == 0)
            output(OL$Verbose, "Done ", i)
    }
    
    t2Metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("t2"))
    metadata <- .StreamlineTractMetadata(FALSE, "vox", t2Metadata)
    
    tract <- .StreamlineSetTract(seed, leftLengths, rightLengths, leftData, rightData, metadata)
    invisible (tract)
}

newStreamlineTractWithMetadata <- function (tract, metadata)
{
    if (!isStreamlineTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineTract object")
    if (!isStreamlineTractMetadata(metadata))
        output(OL$Error, "The specified metadata object is not valid")
    if (!equivalent(tract$getVoxelDimensions(), metadata$getVoxelDimensions(), signMatters=FALSE))
        output(OL$Error, "Can't change voxel dimensions at the moment")
    
    line <- tract$getLine()
    seed <- tract$getOriginalSeedPoint()
    imageMetadata <- tract$getImageMetadata()
    
    if (tract$isOriginAtSeed())
        line <- transformWithTranslation(line, seed)
    
    # Coordinate unit from voxels to millimetres
    if (tract$getCoordinateUnit() == "vox" && metadata$getCoordinateUnit() == "mm")
    {
        line <- transformRVoxelToWorld(line, imageMetadata)
        seed <- transformRVoxelToWorld(seed, imageMetadata)
    }
    else if (tract$getCoordinateUnit() == "mm" && metadata$getCoordinateUnit() == "vox")
    {
        line <- transformWorldToRVoxel(line, imageMetadata)
        seed <- transformWorldToRVoxel(seed, imageMetadata)
    }
    
    if (metadata$isOriginAtSeed())
        line <- transformWithTranslation(line, -seed)
    
    newTract <- .StreamlineTract(line, tract$getSeedIndex(), seed, metadata)
    invisible (newTract)
}

newStreamlineTractFromLine <- function (line, seedIndex, originalSeedPoint, metadata)
{
    axisNames <- c("left-right", "anterior-posterior", "inferior-superior")
    firstStep <- line[seedIndex+1,] - line[seedIndex,]
    testAxis <- which.max(abs(firstStep))
    output(OL$Info, "Using ", axisNames[testAxis], " axis for left/right splitting")
    
    if (firstStep[testAxis] < 0)
    {
        output(OL$Info, "Inverting the order of points in the streamline")
        len <- nrow(line)
        line <- line[len:1,]
        seedIndex <- len - seedIndex + 1
    }
    
    streamline <- .StreamlineTract(line, seedIndex, originalSeedPoint, metadata)
    invisible (streamline)
}

newStreamlineTractFromSet <- function (tract, method = c("median","single"), originAtSeed = NULL, lengthQuantile = NULL, index = NULL)
{
    if (!isStreamlineSetTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineSetTract object")
    if (is.null(originAtSeed))
        originAtSeed <- tract$isOriginAtSeed()
    
    method <- match.arg(method)
    
    if (method == "median")
    {
        if (is.null(lengthQuantile))
            output(OL$Error, "Length quantile must be specified for the \"median\" method")
        
        leftLength <- quantile(tract$getLeftLengths(), probs=lengthQuantile, names=FALSE)
        rightLength <- quantile(tract$getRightLengths(), probs=lengthQuantile, names=FALSE)
    
        leftLine <- apply(tract$getLeftPoints()[1:leftLength,,], 1:2, median, na.rm=TRUE)
        rightLine <- apply(tract$getRightPoints()[1:rightLength,,], 1:2, median, na.rm=TRUE)
    }
    else if (method == "single")
    {
        if (is.null(index))
            output(OL$Error, "Index must be specified for the \"single\" method")
        
        leftLength <- tract$getLeftLengths()[index]
        rightLength <- tract$getRightLengths()[index]
        
        leftLine <- tract$getLeftPoints()[1:leftLength,,index]
        rightLine <- tract$getRightPoints()[1:rightLength,,index]
    }
    
    # Reverse the order of points in the first line part and append it to the
    # second line part, to create the complete median line
    # The first instance of the seed point is removed to avoid duplication
    fullLine <- rbind(leftLine[leftLength:2,], rightLine)
    
    newTract <- .StreamlineTract(fullLine, leftLength, tract$getSeedPoint(), tract$getMetadata())
    finalMetadata <- .StreamlineTractMetadata(originAtSeed, "mm", tract$getImageMetadata())
    newTract <- newStreamlineTractWithMetadata(newTract, finalMetadata)
    
    rm(tract)
    invisible (newTract)
}

newStreamlineTractByTransformation <- function (tract, transform)
{
    if (!isStreamlineTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
    oldSeed <- tract$getOriginalSeedPoint()
    oldLine <- tract$getLine()
    
    if (tract$isOriginAtSeed())
        oldLine <- transformWithTranslation(oldLine, oldSeed)
    
    useVoxels <- (tract$getCoordinateUnit() == "vox")
    newSeed <- transformPointsWithAffine(transform, oldSeed, useVoxels=useVoxels)
    newLine <- transformPointsWithAffine(transform, oldLine, useVoxels=useVoxels)
    
    if (tract$isOriginAtSeed())
        newLine <- transformWithTranslation(newLine, -newSeed)

    newTract <- .StreamlineTract(newLine, tract$getSeedIndex(), newSeed, tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractWithSpacingThreshold <- function (tract, maxSeparation)
{
    if (!isStreamlineTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
    line <- tract$getLine()
    spacings <- tract$getPointSpacings()
    seedPoint <- tract$getSeedIndex()
    nPoints <- tract$nPoints()
    
    wide <- which(spacings > maxSeparation)
    leftWide <- wide[which(wide < seedPoint)]
    rightWide <- wide[which(wide > seedPoint)]
    
    # The following are sensitive to the exact form of the output of
    # StreamlineTract$getPointSpacings() - at present, there is no leading NA
    # in the spacing vector, so spacings[1] is the distance *from* the first
    # point on the line
    leftStop <- ifelse(length(leftWide) > 0, max(leftWide)+1, 1)
    rightStop <- ifelse(length(rightWide) > 0, min(rightWide), nPoints)
    
    if (!identical(c(leftStop,rightStop), c(1,nPoints)))
        output(OL$Info, "Truncating streamline to avoid large space between points")
    
    newTract <- .StreamlineTract(line[leftStop:rightStop,], (seedPoint-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractWithCurvatureThreshold <- function (tract, maxAngle, isRadians = FALSE)
{
    if (!isStreamlineTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
    if (!isRadians)
        maxAngle <- maxAngle / 180 * pi
    
    line <- tract$getLine()
    seedPoint <- tract$getSeedIndex()
    nPoints <- tract$nPoints()
    steps <- characteriseStepVectors(line, seedPoint)
    
    leftSharp <- which(steps$leftAngles > maxAngle)
    rightSharp <- which(steps$rightAngles > maxAngle)
    
    leftStop <- ifelse(length(leftSharp) > 0, max(leftSharp), 1)
    rightStop <- ifelse(length(rightSharp) > 0, min(rightSharp)-1, nPoints)
    
    if (!identical(c(leftStop,rightStop), c(1,nPoints)))
        output(OL$Info, "Truncating streamline to avoid sharp curvature")
    
    newTract <- .StreamlineTract(line[leftStop:rightStop,], (seedPoint-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractByTrimming <- function (tract, trimLeft, trimRight)
{
    if (!isStreamlineTract(tract))
        output(OL$Error, "The specified tract is not a valid StreamlineTract object")
    if (tract$getCoordinateUnit() != "mm")
        output(OL$Error, "This function requires a tract which uses a world coordinate system")
    
    line <- tract$getLine()
    spacings <- tract$getPointSpacings()
    
    leftSum <- cumsum(spacings)
    rightSum <- cumsum(rev(spacings))
    
    leftStop <- ifelse(max(leftSum) > trimLeft, min(which(leftSum > trimLeft)), 1)
    rightStop <- tract$nPoints() - ifelse(max(rightSum) > trimRight, min(which(rightSum > trimRight))-1, 0)
    
    newTract <- .StreamlineTract(line[leftStop:rightStop,], (tract$getSeedIndex()-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

rescalePoints <- function (points, newUnit, metadata, seed)
{
    oldUnit <- metadata$getCoordinateUnit()
    imageMetadata <- metadata$getImageMetadata()
    
    if (!is.null(newUnit) && (newUnit != oldUnit))
    {
        if (metadata$isOriginAtSeed())
            points <- transformWithTranslation(points, seed)

        if (oldUnit == "vox" && newUnit == "mm")
        {
            points <- transformRVoxelToWorld(points, imageMetadata)
            seed <- transformRVoxelToWorld(seed, imageMetadata)
        }
        else if (oldUnit == "mm" && newUnit == "vox")
        {
            points <- transformWorldToRVoxel(points, imageMetadata)
            seed <- transformWorldToRVoxel(seed, imageMetadata)
        }

        if (metadata$isOriginAtSeed())
            points <- transformWithTranslation(points, -seed)
    }
    
    invisible (list(seed=seed, points=points))
}

getAxesForStreamlinePlot <- function (x, unit = NULL, axes = NULL, drawAxes = FALSE)
{
    if (is.null(unit))
        unit <- x$getCoordinateUnit()
    
    output(OL$Info, "Calculating plot range")
    range <- x$getSpatialRange()
    rescaledMaxes <- rescalePoints(range$maxes, unit, x$getMetadata(), x$getSeedPoint())
    rescaledMins <- rescalePoints(range$mins, unit, x$getMetadata(), x$getSeedPoint())
    range$maxes <- rescaledMaxes$points
    range$mins <- rescaledMins$points
    
    if (is.null(axes))
    {
        rangeWidths <- range$maxes - range$mins
        axes <- setdiff(1:3, which.min(rangeWidths))
    }
    
    if (length(axes) != 2)
        output(OL$Error, "Exactly two axes must be specified")
    
    axisNames <- c("left-right", "anterior-posterior", "inferior-superior")
    xlim <- c(range$mins[axes[1]], range$maxes[axes[1]])
    ylim <- c(range$mins[axes[2]], range$maxes[axes[2]])
    
    if (drawAxes)
    {
        xlab <- paste(axisNames[axes[1]], " (", unit, ")", sep="")
        ylab <- paste(axisNames[axes[2]], " (", unit, ")", sep="")
        plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, asp=1)
    }
    
    return(axes)
}

plot.tract.streamline <- function (x, y = NULL, unit = NULL, axes = NULL, add = FALSE, ...)
{
    if (add)
    {
        if (is.null(axes))
            output(OL$Error, "Axes must be specified if adding to an existing plot")
    }
    else    
        axes <- getAxesForStreamlinePlot(x, unit, axes, drawAxes=TRUE)
    
    line <- x$getLine()
    rescaledLine <- rescalePoints(line, unit, x$getMetadata(), x$getSeedPoint())
    lines(rescaledLine$points[,axes[1]], rescaledLine$points[,axes[2]], ...)
    
    invisible (axes)
}

plot.tract.streamline.set <- function (x, y = NULL, unit = NULL, axes = NULL, add = FALSE, ...)
{
    if (add)
    {
        if (is.null(axes))
            output(OL$Error, "Axes must be specified if adding to an existing plot")
    }
    else
        axes <- getAxesForStreamlinePlot(x, unit, axes, drawAxes=TRUE)
    
    output(OL$Info, "Plotting streamlines")
    leftPoints <- x$getLeftPoints()
    rightPoints <- x$getRightPoints()
    
    for (i in 1:x$nStreamlines())
    {
        ll <- rescalePoints(leftPoints[,,i], unit, x$getMetadata(), x$getSeedPoint())
        rl <- rescalePoints(rightPoints[,,i], unit, x$getMetadata(), x$getSeedPoint())
        lines(ll$points[,axes[1]], ll$points[,axes[2]], ...)
        lines(rl$points[,axes[1]], rl$points[,axes[2]], ...)
    }
    
    if (x$isOriginAtSeed())
        seed <- rep(0,3)
    else
        seed <- x$getSeedPoint()
    
    points(seed[axes[1]], seed[axes[2]], col="red", pch=19)
    
    invisible (axes)
}
