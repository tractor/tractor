StreamlineTractMetadata <- setRefClass("StreamlineTractMetadata", contains="SerialisableObject", fields=list(originAtSeed="logical",coordUnit="character",imageMetadata="MriImage"), methods=list(
    initialize = function (...)
    {
        object <- initFields(...)
        if (length(object$coordUnit) > 0 && !isTRUE(object$coordUnit %in% c("vox","mm")))
            report(OL$Error, "Coordinate unit must be \"vox\" (for voxels) or \"mm\" (for millimetres)")
        
        return (object)
    },
    
    getCoordinateUnit = function () { return (coordUnit) },
    
    getImageMetadata = function () { return (imageMetadata) },
    
    getSpatialRange = function () { report(OL$Error, "Method \"getSpatialRange\" undefined") },
    
    isOriginAtSeed = function () { return (originAtSeed) }
))

StreamlineTract <- setRefClass("StreamlineTract", contains="StreamlineTractMetadata", fields=list(line="matrix",seedIndex="integer",originalSeedPoint="numeric",pointSpacings="numeric"), methods=list(
    initialize = function (line = matrix(), seedIndex = NULL, originalSeedPoint = NULL, metadata = NULL, ...)
    {
        if (!is.null(metadata))
            import(metadata, "StreamlineTractMetadata")
        else
            callSuper(...)
        
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), originalSeedPoint=as.numeric(originalSeedPoint))

        if (dim(object$line)[2] != 3)
            report(OL$Error, "Streamline must be specified as a matrix with 3 columns")        
        if (dim(object$line)[1] == 1)
            object$pointSpacings <- numeric(0)
        else
            object$pointSpacings <- apply(diff(object$line), 1, vectorLength)
        
        return (object)
    },
    
    getLine = function () { return (line) },
    
    getLineLength = function () { return (sum(pointSpacings)) },
    
    getMetadata = function () { return (export("StreamlineTractMetadata")) },
    
    getPointSpacings = function () { return (pointSpacings) },
    
    getSeedIndex = function () { return (seedIndex) },
    
    getOriginalSeedPoint = function () { return (originalSeedPoint) },
    
    getSeedPoint = function () { return (originalSeedPoint) },
    
    getSpatialRange = function ()
    {
        fullRange <- apply(line, 2, range, na.rm=TRUE)
        return (list(mins=fullRange[1,], maxes=fullRange[2,]))
    },
    
    nPoints = function () { return (dim(line)[1]) }
))

StreamlineSetTract <- setRefClass("StreamlineSetTract", contains="StreamlineTractMetadata", fields=list(seedPoint="numeric",leftLengths="integer",rightLengths="integer",leftPoints="array",rightPoints="array"), methods=list(
    initialize = function (..., metadata = NULL)
    {
        if (!is.null(metadata))
            import(metadata, "StreamlineTractMetadata")
        return (initFields(...))
    },
    
    getLeftLengths = function () { return (leftLengths) },
    
    getLeftPoints = function () { return (leftPoints) },
    
    getMetadata = function () { return (export("StreamlineTractMetadata")) },
    
    getRightLengths = function () { return (rightLengths) },
    
    getRightPoints = function () { return (rightPoints) },
    
    getSeedPoint = function () { return (seedPoint) },
    
    getSpatialRange = function ()
    {
        leftRange <- apply(leftPoints, 2, range, na.rm=TRUE)
        rightRange <- apply(rightPoints, 2, range, na.rm=TRUE)
        mins <- pmin(leftRange,rightRange)[1,]
        maxes <- pmax(leftRange,rightRange)[2,]
        return (list(mins=mins, maxes=maxes))
    },
    
    nStreamlines = function () { return (dim(leftPoints)[3]) }
))

StreamlineCollectionTract <- setRefClass("StreamlineCollectionTract", contains="StreamlineTractMetadata", fields=list(seedIndices="integer",startIndices="integer",points="matrix"), methods=list(
    initialize = function (..., metadata = NULL)
    {
        if (!is.null(metadata))
            import(metadata, "StreamlineTractMetadata")
        return (initFields(...))
    },
    
    getMetadata = function () { return (export("StreamlineTractMetadata")) },
    
    getEndIndex = function (i = 1) { return (.self$getEndIndices()[i]) },
    
    getEndIndices = function () { return (c(startIndices[-1]-1, nrow(points))) },
    
    getPoints = function (i = NA)
    {
        if (is.na(i))
            return (points)
        else if (i == .self$nStreamlines())
            return (points[startIndices[i]:nrow(points),,drop=FALSE])
        else
            return (points[startIndices[i]:(startIndices[i+1]-1),,drop=FALSE])
    },
    
    getSeedIndex = function (i = 1) { return (seedIndices[i]) },
    
    getSeedIndices = function () { return (seedIndices) },
    
    getSeedPoint = function (i = 1) { return (points[seedIndices[i],]) },                                                         
    
    getSeedPoints = function () { return (points[seedIndices,]) },
    
    getSpatialRange = function ()
    {
        fullRange <- apply(points, 2, range, na.rm=TRUE)
        return (list(mins=fullRange[1,], maxes=fullRange[2,]))
    },
    
    getStartIndex  = function (i = 1) { return (startIndices[i]) },
    
    getStartIndices  = function () { return (startIndices) },
    
    nPoints = function () { return (nrow(points)) },
    
    nStreamlines = function () { return (length(startIndices)) },
    
    summarise = function ()
    {
        # Note, length is number of steps, not number of points
        lengths <- .self$getEndIndices() - .self$getStartIndices()
        index <- which(lengths > 1)[1]
        step <- diff(.self$getPoints(index))[1,]
        if (.self$getCoordinateUnit() == "mm")
            stepLength <- vectorLength(step)
        else
            stepLength <- vectorLength(step * .self$getImageMetadata()$getVoxelDimensions())
        
        labels <- c("Number of streamlines", "Step length", "Streamline length range")
        values <- c(.self$nStreamlines(), paste(round(stepLength,3),"mm"), paste(implode(range(lengths)," to "),"steps"))
        return (list(labels=labels, values=values))
    }
))

newStreamlineTractMetadataFromImageMetadata <- function (imageMetadata, originAtSeed, coordUnit)
{
    if (!is(imageMetadata, "MriImage"))
        report(OL$Error, "The specified image metadata is not an MriImage object")
    
    imageMetadata <- imageMetadata$copy()
    imageMetadata$stripData()
    
    tractMetadata <- StreamlineTractMetadata$new(originAtSeed=originAtSeed, coordUnit=coordUnit, imageMetadata=imageMetadata)
    invisible (tractMetadata)
}

newStreamlineSetTractFromCollection <- function (tract)
{
    if (!is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineCollectionTract object")
    
    points <- tract$getPoints()
    seedPoints <- tract$getSeedPoints()
    if (!all(apply(seedPoints, 1, equivalent, seedPoints[1,])))
        report(OL$Error, "All seed points must be the same")
    
    seedIndices <- tract$getSeedIndices()
    startIndices <- tract$getStartIndices()
    endIndices <- tract$getEndIndices()
    leftLengths <- seedIndices - startIndices + 1
    rightLengths <- endIndices - seedIndices + 1
    
    leftPoints <- array(NA, dim=c(max(leftLengths),3,tract$nStreamlines()))
    rightPoints <- array(NA, dim=c(max(rightLengths),3,tract$nStreamlines()))
    for (i in seq_len(tract$nStreamlines()))
    {
        leftPoints[1:leftLengths[i],,i] <- points[seedIndices[i]:startIndices[i],]
        rightPoints[1:rightLengths[i],,i] <- points[seedIndices[i]:endIndices[i],]
    }
    
    newTract <- StreamlineSetTract$new(seedPoint=seedPoints[1,], leftLengths=as.integer(leftLengths), rightLengths=as.integer(rightLengths), leftPoints=leftPoints, rightPoints=rightPoints, metadata=tract$getMetadata())
    invisible(newTract)
}

newStreamlineSetTractFromProbtrack <- function (session, x, y = NULL, z = NULL, nSamples = 5000, maxPathLength = NULL, rightwardsVector = NULL)
{
    probtrackResult <- runProbtrackWithSession(session, x, y, z, requireParticlesDir=TRUE, nSamples=nSamples, verbose=TRUE, force=TRUE)
    
    seed <- drop(probtrackResult$seeds)
    axisNames <- c("left-right", "anterior-posterior", "inferior-superior")
    
    if (is.null(maxPathLength))
    {
        fileSizes <- particleFileSizesForResult(probtrackResult)
        maxPathLength <- round(max(fileSizes) / 20)
        report(OL$Info, "Setting maximum path length to ", maxPathLength)
    }
    
    if (!is.null(rightwardsVector) && (!is.numeric(rightwardsVector) || length(rightwardsVector) != 3))
    {
        flag(OL$Warning, "Rightwards vector specified is not a numeric 3-vector - ignoring it")
        rightwardsVector <- NULL
    }
    
    subSize <- ifelse(nSamples < 40, nSamples, ceiling(nSamples / 20))
    
    if (is.null(rightwardsVector))
    {
        subData <- array(NA, dim=c(maxPathLength,3,subSize))
        report(OL$Info, "Reading subset of ", subSize, " streamlines")

        for (i in 1:subSize)
        {
            sampleData <- retrieveProbtrackStreamline(probtrackResult, i)
            len <- length(sampleData[,1])

            if (len > maxPathLength)
            {
                report(OL$Warning, "Path length for streamline ", i, " is too long (", len, "); truncating")
                len <- maxPathLength
            }

            subData[1:len,,i] <- sampleData[1:len,]
        }

        # The estimated rightwards direction is the mean first step
        firstSteps <- subData[2,,] - subData[1,,]
        rightwardsVector <- apply(abs(firstSteps), 1, mean)
        
        # Remove the subsample to save memory
        rm(subData)
    }
    
    report(OL$Info, "Rightwards vector is (", implode(round(rightwardsVector,2),","), ")")
    
    leftLengths <- numeric(0)
    rightLengths <- numeric(0)
    leftData <- array(NA, dim=c(maxPathLength,3,nSamples))
    rightData <- array(NA, dim=c(maxPathLength,3,nSamples))
    report(OL$Info, "Reading data from ", nSamples, " streamlines...")
    
    # Read all the data in, separating out "left" and "right" streamline parts
    for (i in 1:nSamples)
    {       
        sampleData <- retrieveProbtrackStreamline(probtrackResult, i)
        len <- length(sampleData[,1])
        
        # If any streamline is too long, we have to truncate it
        if (len > maxPathLength)
        {
            report(OL$Warning, "Path length for streamline ", i, " (", len, ") is too long; truncating")
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
            report(OL$Error, "Seed point appears ", length(startRows), " times (expecting 2) in streamline ", i)
        restartRow <- startRows[2]
        if (restartRow > len)
            report(OL$Error, "Left and right line components do not appear within the specified maximum path length")
        
        rightSideInnerProduct <- (sampleData[2,]-sampleData[1,]) %*% rightwardsVector
        if (rightSideInnerProduct == 0)
            flag(OL$Warning, "Tract is orthogonal to the left-right separation plane")
        
        if (rightSideInnerProduct <= 0)
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
            report(OL$Verbose, "Done ", i)
    }
    
    t2Metadata <- session$getImageByType("maskedb0", metadataOnly=TRUE)
    metadata <- newStreamlineTractMetadataFromImageMetadata(t2Metadata, FALSE, "vox")
    
    tract <- StreamlineSetTract$new(seedPoint=seed, leftLengths=as.integer(leftLengths), rightLengths=as.integer(rightLengths), leftPoints=leftData, rightPoints=rightData, metadata=metadata)
    invisible (tract)
}

newStreamlineSetTractBySubsetting <- function (tract, indices)
{
    if (!is(tract, "StreamlineSetTract"))
        report(OL$Error, "The specified tract is not a StreamlineSetTract object")
    if (length(indices) < 1)
        report(OL$Error, "At least one streamline must be included in the subset")
    
    leftLengths <- tract$getLeftLengths()[indices]
    rightLengths <- tract$getRightLengths()[indices]
    leftPoints <- tract$getLeftPoints()[,,indices,drop=FALSE]
    rightPoints <- tract$getRightPoints()[,,indices,drop=FALSE]
    
    newTract <- StreamlineSetTract$new(seedPoint=tract$getSeedPoint(), leftLengths=as.integer(leftLengths), rightLengths=as.integer(rightLengths), leftPoints=leftPoints, rightPoints=rightPoints, metadata=tract$getMetadata())
    invisible (newTract)
}

newStreamlineSetTractFromStreamline <- function (tract)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a StreamlineTract object")
    
    line <- tract$getLine()
    
    leftLength <- tract$getSeedIndex()
    rightLength <- nrow(line) - leftLength + 1
    leftPoints <- line[leftLength:1,]
    rightPoints <- line[leftLength:nrow(line),]
    dim(leftPoints) <- c(dim(leftPoints), 1)
    dim(rightPoints) <- c(dim(rightPoints), 1)
    
    newTract <- StreamlineSetTract$new(seedPoint=tract$getSeedPoint(), leftLengths=as.integer(leftLength), rightLengths=as.integer(rightLength), leftPoints=leftPoints, rightPoints=rightPoints, metadata=tract$getMetadata())
    invisible (newTract)
}

newStreamlineSetTractByTruncationToReference <- function (tract, reference, testSession)
{
    if (!is(tract, "StreamlineSetTract"))
        report(OL$Error, "The specified tract is not a StreamlineSetTract object")
    if (!is(reference,"ReferenceTract") || !is(reference$getTract(),"BSplineTract"))
        report(OL$Error, "The specified reference tract is not valid")
    
    # Transform the reference tract into native space, find its length in this
    # space, and use that to truncate the streamline set
    refSession <- reference$getSourceSession()
    refPoints <- getPointsForTract(reference$getTract(), reference$getTractOptions()$pointType)
    
    if (reference$getTractOptions()$registerToReference)
    {
        if (is.null(refSession))
            transform <- getMniTransformForSession(testSession)
        else
            transform <- newAffineTransform3DFromFlirt(refSession$getImageFileNameByType("maskedb0"), testSession$getImageFileNameByType("maskedb0"))
    
        refPoints$points <- transformWorldPointsWithAffine(transform, refPoints$points)
    }
    
    refSteps <- calculateStepVectors(refPoints$points, refPoints$seedPoint)
    refLeftLength <- sum(apply(refSteps$left,1,vectorLength), na.rm=TRUE)
    refRightLength <- sum(apply(refSteps$right,1,vectorLength), na.rm=TRUE)
    
    # Estimate the step length from the mean of the first ten gaps in the first streamline
    nTestPoints <- min(10, max(tract$getLeftLengths()))
    testStreamline <- which(tract$getLeftLengths() >= nTestPoints)[1]
    testPoints <- tract$getLeftPoints()[1:nTestPoints,,testStreamline]
    if (tract$getCoordinateUnit() == "vox")
        testPoints <- transformRVoxelToWorld(testPoints, tract$getImageMetadata())
    realStepLength <- mean(apply(diff(testPoints), 1, vectorLength), na.rm=TRUE)
    report(OL$Info, "Step length in streamline set is ", signif(realStepLength,3), " mm")
    
    maxPointsLeft <- ceiling(refLeftLength / realStepLength)
    maxPointsRight <- ceiling(refRightLength / realStepLength)
    
    leftLengths <- pmin(maxPointsLeft, tract$getLeftLengths())
    rightLengths <- pmin(maxPointsRight, tract$getRightLengths())
    leftPoints <- tract$getLeftPoints()[1:max(leftLengths),,,drop=FALSE]
    rightPoints <- tract$getRightPoints()[1:max(rightLengths),,,drop=FALSE]
    
    newTract <- StreamlineSetTract$new(seedPoint=tract$getSeedPoint(), leftLengths=as.integer(leftLengths), rightLengths=as.integer(rightLengths), leftPoints=leftPoints, rightPoints=rightPoints, metadata=tract$getMetadata())
    invisible (newTract)
}

newStreamlineCollectionTractBySubsetting <- function (tract, indices)
{
    if (!is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineCollectionTract object")
    if (length(indices) < 1)
        report(OL$Error, "At least one streamline must be included in the subset")
    
    seedIndices <- tract$getSeedIndices()[indices]
    startIndices <- tract$getStartIndices()[indices]
    endIndices <- tract$getEndIndices()[indices]
    pointIndices <- unlist(mapply(seq, startIndices, endIndices))
    
    newTract <- StreamlineCollectionTract$new(points=tract$getPoints()[pointIndices,,drop=FALSE], seedIndices=match(seedIndices,pointIndices), startIndices=match(startIndices,pointIndices), metadata=tract$getMetadata())
    invisible (newTract)
}

newStreamlineCollectionTractWithLengthThreshold <- function (tract, threshold = 0, unit = c("step","mm"))
{
    if (!is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineCollectionTract object")
    
    unit <- match.arg(unit)
    if (unit == "step")
        steps <- threshold
    else
    {
        rescaled <- rescalePoints(tract$getPoints(1), unit, tract$getMetadata(), tract$getSeedPoint(1))
        stepLength <- vectorLength(diff(rescaled$points)[1,])
        steps <- ceiling(threshold / stepLength)
    }
    
    streamlineLengths <- diff(c(tract$getStartIndices(), tract$nPoints()+1))
    toKeep <- which(streamlineLengths >= steps)
    newTract <- newStreamlineCollectionTractBySubsetting(tract, toKeep)
    invisible (newTract)
}

newStreamlineTractWithMetadata <- function (tract, metadata)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineTract object")
    if (!is(metadata, "StreamlineTractMetadata"))
        report(OL$Error, "The specified metadata object is not valid")
    if (!equivalent(tract$getImageMetadata()$getVoxelDimensions(), metadata$getImageMetadata()$getVoxelDimensions(), signMatters=FALSE))
        report(OL$Error, "Can't change voxel dimensions at the moment")
    
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
    
    newTract <- newStreamlineTractFromLine(line, tract$getSeedIndex(), seed, metadata)
    invisible (newTract)
}

newStreamlineTractFromLine <- function (line, seedIndex, originalSeedPoint, metadata)
{
    streamline <- StreamlineTract$new(line=line, seedIndex=as.integer(seedIndex), originalSeedPoint=originalSeedPoint, metadata=metadata)
    invisible (streamline)
}

newStreamlineTractFromSet <- function (tract, method = c("median","single"), originAtSeed = NULL, lengthQuantile = NULL, index = NULL)
{
    if (!is(tract, "StreamlineSetTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineSetTract object")
    if (is.null(originAtSeed))
        originAtSeed <- tract$isOriginAtSeed()
    
    method <- match.arg(method)
    
    if (method == "median")
    {
        if (is.null(lengthQuantile))
            report(OL$Error, "Length quantile must be specified for the \"median\" method")
        
        leftLength <- floor(quantile(tract$getLeftLengths(), probs=lengthQuantile, names=FALSE))
        rightLength <- floor(quantile(tract$getRightLengths(), probs=lengthQuantile, names=FALSE))
        
        leftLine <- apply(tract$getLeftPoints()[1:leftLength,,,drop=FALSE], 1:2, median, na.rm=TRUE)
        rightLine <- apply(tract$getRightPoints()[1:rightLength,,,drop=FALSE], 1:2, median, na.rm=TRUE)
    }
    else if (method == "single")
    {
        if (is.null(index))
            report(OL$Error, "Index must be specified for the \"single\" method")
        
        leftLength <- tract$getLeftLengths()[index]
        rightLength <- tract$getRightLengths()[index]
        
        leftLine <- tract$getLeftPoints()[1:leftLength,,index]
        rightLine <- tract$getRightPoints()[1:rightLength,,index]
    }
    
    # Reverse the order of points in the first line part and append it to the
    # second line part, to create the complete median line
    # The first instance of the seed point is removed to avoid duplication
    if (leftLength < 2)
        fullLine <- rightLine
    else
        fullLine <- rbind(leftLine[leftLength:2,], rightLine)
    
    newTract <- newStreamlineTractFromLine(fullLine, leftLength, tract$getSeedPoint(), tract$getMetadata())
    finalMetadata <- StreamlineTractMetadata$new(originAtSeed=originAtSeed, coordUnit="mm", imageMetadata=tract$getImageMetadata())
    newTract <- newStreamlineTractWithMetadata(newTract, finalMetadata)
    
    rm(tract)
    invisible (newTract)
}

newStreamlineTractByTransformation <- function (tract, transform)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
    oldSeed <- tract$getOriginalSeedPoint()
    oldLine <- tract$getLine()
    
    if (tract$isOriginAtSeed())
        oldLine <- transformWithTranslation(oldLine, oldSeed)
    
    useVoxels <- (tract$getCoordinateUnit() == "vox")
    newSeed <- transformPointsWithAffine(transform, oldSeed, useVoxels=useVoxels)
    newLine <- transformPointsWithAffine(transform, oldLine, useVoxels=useVoxels)
    
    if (tract$isOriginAtSeed())
        newLine <- transformWithTranslation(newLine, -newSeed)

    newTract <- newStreamlineTractFromLine(newLine, tract$getSeedIndex(), newSeed, tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractWithSpacingThreshold <- function (tract, maxSeparation)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
    line <- tract$getLine()
    spacings <- tract$getPointSpacings()
    seedPoint <- tract$getSeedIndex()
    nPoints <- tract$nPoints()

    if (length(spacings) == 0)
        return (invisible(tract))
    
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
        flag(OL$Info, "Truncating streamline to avoid large space between points")
    
    newTract <- newStreamlineTractFromLine(line[leftStop:rightStop,], (seedPoint-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractWithCurvatureThreshold <- function (tract, maxAngle, isRadians = FALSE)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineTract object")
    
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
        report(OL$Info, "Truncating streamline to avoid sharp curvature")
    
    newTract <- newStreamlineTractFromLine(line[leftStop:rightStop,], (seedPoint-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

newStreamlineTractByTrimming <- function (tract, trimLeft, trimRight)
{
    if (!is(tract, "StreamlineTract"))
        report(OL$Error, "The specified tract is not a valid StreamlineTract object")
    if (tract$getCoordinateUnit() != "mm")
        report(OL$Error, "This function requires a tract which uses a world coordinate system")
    
    line <- tract$getLine()
    spacings <- tract$getPointSpacings()
    
    leftSum <- cumsum(spacings)
    rightSum <- cumsum(rev(spacings))
    
    leftStop <- ifelse(max(leftSum) > trimLeft, min(which(leftSum > trimLeft))+1, 1)
    rightStop <- tract$nPoints() - ifelse(max(rightSum) > trimRight, min(which(rightSum > trimRight)), 0)
    
    newTract <- newStreamlineTractFromLine(line[leftStop:rightStop,], (tract$getSeedIndex()-leftStop+1), tract$getOriginalSeedPoint(), tract$getMetadata())
    invisible (newTract)
}

writeStreamlineCollectionTractToTrackvis <- function (tract, fileName)
{
    if (!is(tract, "StreamlineCollectionTract"))
        report(OL$Error, "The specified tract is not a StreamlineCollectionTract object")
    
    imageMetadata <- tract$getImageMetadata()
    fullVoxelDims <- c(-1, abs(imageMetadata$getVoxelDimensions()), rep(0,7-imageMetadata$getDimensionality()))
    origin <- (imageMetadata$getOrigin() - 1) * abs(imageMetadata$getVoxelDimensions())
    if (length(origin) > 3)
        origin <- origin[1:3]
    else if (length(origin) < 3)
        origin <- c(origin, rep(0,3-length(origin)))
    origin <- ifelse(origin < 0, rep(0,3), origin)
    origin[2:3] <- -origin[2:3]
    xformMatrix <- matrix(c(-fullVoxelDims[2], 0, 0, origin[1],
                             0, fullVoxelDims[3], 0, origin[2],
                             0, 0, fullVoxelDims[4], origin[3],
                             0, 0, 0,                1        ), nrow=4, ncol=4, byrow=TRUE)
    
    fileName <- ensureFileSuffix(fileName, "trk")
    connection <- file(fileName, "w+b")
    
    # File header
    writeBin(c(charToRaw("TRACK"),as.raw(0)), connection, size=1)
    writeBin(as.integer(imageMetadata$getDimensions()[1:3]), connection, size=2)
    writeBin(abs(imageMetadata$getVoxelDimensions()[1:3]), connection, size=4)
    
    # The origin is not currently used by TrackVis, according to the docs (http://www.trackvis.org/docs/?subsect=fileformat)
    writeBin(origin, connection, size=4)
    
    # Number of scalars saved at each track point (besides x, y and z coordinates), and their names
    writeBin(as.integer(0), connection, size=2)
    writeBin(raw(200), connection)

    # Number of properties saved at each track, and their names
    writeBin(as.integer(0), connection, size=2)
    writeBin(raw(200), connection)
    
    writeBin(as.vector(xformMatrix), connection, size=4)
    writeBin(raw(444), connection)
    writeBin(c(charToRaw("LAS"),as.raw(0)), connection, size=1)
    writeBin(c(charToRaw("LAS"),as.raw(0)), connection, size=1)
    writeBin(as.double(c(1,0,0,0,-1,0)), connection, size=4)
    writeBin(raw(2), connection)
    
    # Invert and swap bits - all zero
    writeBin(raw(6), connection)
    
    # Number of streamlines
    writeBin(as.integer(tract$nStreamlines()), connection, size=4)
    
    # Version and header size
    writeBin(as.integer(c(2, 1000)), connection, size=4)
    
    # Write out streamline points
    lengths <- tract$getEndIndices() - tract$getStartIndices() + 1
    for (i in seq_len(tract$nStreamlines()))
    {
        rescaled <- rescalePoints(tract$getPoints(i), "mm", tract$getMetadata(), tract$getSeedPoint(i))
        writeBin(as.integer(lengths[i]), connection, size=4)
        writeBin(as.vector(t(rescaled$points),"double"), connection, size=4)
    }
    
    close(connection)
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
    
    report(OL$Info, "Calculating plot range")
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
        report(OL$Error, "Exactly two axes must be specified")
    
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

plot.StreamlineTract <- function (x, y = NULL, unit = NULL, axes = NULL, add = FALSE, ...)
{
    if (!add)
        axes <- getAxesForStreamlinePlot(x, unit, axes, drawAxes=TRUE)
    else if (is.null(axes))
        report(OL$Error, "Axes must be specified if adding to an existing plot")
    
    line <- x$getLine()
    rescaledLine <- rescalePoints(line, unit, x$getMetadata(), x$getSeedPoint())
    lines(rescaledLine$points[,axes[1]], rescaledLine$points[,axes[2]], ...)
    
    invisible (axes)
}

plot.StreamlineSetTract <- function (x, y = NULL, unit = NULL, axes = NULL, add = FALSE, ...)
{
    if (!add)
        axes <- getAxesForStreamlinePlot(x, unit, axes, drawAxes=TRUE)
    else if (is.null(axes))
        report(OL$Error, "Axes must be specified if adding to an existing plot")
    
    report(OL$Info, "Plotting streamlines")
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

plot.StreamlineCollectionTract <- function (x, y = NULL, unit = NULL, axes = NULL, add = FALSE, ...)
{
    if (!add)
        axes <- getAxesForStreamlinePlot(x, unit, axes, drawAxes=TRUE)
    else if (is.null(axes))
        report(OL$Error, "Axes must be specified if adding to an existing plot")
    
    points <- x$getPoints()
    seedIndices <- x$getSeedIndices()
    startIndices <- x$getStartIndices()
    endIndices <- c(startIndices[-1]-1, nrow(points))
    
    report(OL$Info, "Plotting streamlines")
    for (i in 1:x$nStreamlines())
    {
        if (endIndices[i] > startIndices[i])
        {
            line <- points[startIndices[i]:endIndices[i],]
            rescaledLine <- rescalePoints(line, unit, x$getMetadata(), points[seedIndices[i],])
            lines(rescaledLine$points[,axes[1]], rescaledLine$points[,axes[2]], ...)
        }
    }
    
    invisible (axes)
}
