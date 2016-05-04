BSplineTract <- setRefClass("BSplineTract", contains="SerialisableObject", fields=list(splineDegree="integer",splineModels="list",knotPositions="numeric",knotLocations="matrix",seedKnot="integer"), methods=list(
    initialize = function (...)
    {
        object <- initFields(...)
        
        knotSpacings <- diff(knotPositions)
        if (length(knotSpacings) > 0 && !equivalent(knotSpacings, rep(knotSpacings[1],length(knotSpacings))))
            report(OL$Error, "Knots are not equally spaced")
        
        return (object)
    },
    
    getControlPoints = function ()
    {
        nKnots <- .self$nKnots()
        controlPoints <- array(NA, dim=c(nKnots+splineDegree,3))
        indices <- 2:(nKnots+splineDegree+1)
        controlPoints[,1] <- splineModels[[1]]$coefficients[indices] + splineModels[[1]]$coefficients[1]
        controlPoints[,2] <- splineModels[[2]]$coefficients[indices] + splineModels[[2]]$coefficients[1]
        controlPoints[,3] <- splineModels[[3]]$coefficients[indices] + splineModels[[3]]$coefficients[1]
        return (controlPoints)
    },
    
    getKnotLocations = function () { return (knotLocations) },
    
    getKnotPositions = function () { return (knotPositions) },
    
    getKnotSpacing = function () { return (diff(knotPositions)[1]) },
    
    getLineAtPoints = function (tValues)
    {
        locsX <- as.vector(predict(splineModels[[1]], data.frame(t=tValues)))
        locsY <- as.vector(predict(splineModels[[2]], data.frame(t=tValues)))
        locsZ <- as.vector(predict(splineModels[[3]], data.frame(t=tValues)))
        return (matrix(c(locsX, locsY, locsZ), ncol=3))
    },
    
    getSeedControlPoint = function () { return (seedKnot+1) },
    
    getSeedKnot = function () { return (seedKnot) },
    
    nControlPoints = function () { return (.self$nKnots() + splineDegree) },
    
    nKnots = function () { return (length(knotPositions)) }
))

plot.BSplineTract <- function (x, y = NULL, axes = NULL, add = FALSE, ...)
{
    tRange <- range(x$getKnotPositions())
    line <- x$getLineAtPoints(seq(tRange[1], tRange[2], length.out=100))
    knotLocs <- x$getKnotLocations()
    seedKnot <- x$getSeedKnot()
    
    fullRange <- apply(line, 2, range, na.rm=TRUE)
    fullRange <- list(mins=fullRange[1,], maxes=fullRange[2,])
    
    if (is.null(axes))
    {
        if (add)
            report(OL$Error, "Axes must be specified if adding to an existing plot")
        
        rangeWidths <- fullRange$maxes - fullRange$mins
        axes <- setdiff(1:3, which.min(rangeWidths))
    }
    else if (length(axes) != 2)
        report(OL$Error, "Exactly two axes must be specified")
    
    axisNames <- c("left-right", "anterior-posterior", "inferior-superior")
    xlim <- c(fullRange$mins[axes[1]], fullRange$maxes[axes[1]])
    ylim <- c(fullRange$mins[axes[2]], fullRange$maxes[axes[2]])
    
    if (!add)
    {
        xlab <- paste(axisNames[axes[1]], " (mm)", sep="")
        ylab <- paste(axisNames[axes[2]], " (mm)", sep="")
        plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, asp=1)
    }
    
    lines(line[,axes[1]], line[,axes[2]], lwd=1, col="grey60")
    points(knotLocs[,axes[1]], knotLocs[,axes[2]], lwd=2, pch=19, type="b", ...)
    points(knotLocs[seedKnot,axes[1]], knotLocs[seedKnot,axes[2]], cex=2)
    
    invisible (axes)
}

newBSplineTractFromStreamline <- function (streamlineTract, knotSpacing = NULL, maxResidError = 0.1)
{
    fitBSplineModels <- function (streamlineTract, nKnots = NULL, gap = NULL)
    {
        # All of the following numbers are parametric, in mm along the line
        lineLength <- streamlineTract$getLineLength()
        pointLocs <- c(0, cumsum(streamlineTract$getPointSpacings()))
        seedLoc <- pointLocs[streamlineTract$getSeedIndex()]
        
        if (is.null(gap))
        {
            beforeSeedFraction <- seedLoc / lineLength
            nBeforeSeedKnots <- floor(nKnots * beforeSeedFraction)
            nAfterSeedKnots <- floor(nKnots * (1-beforeSeedFraction))
            gap <- min(seedLoc / nBeforeSeedKnots, (lineLength-seedLoc) / nAfterSeedKnots, lineLength)
            knots <- seedLoc + (-nBeforeSeedKnots:nAfterSeedKnots * gap)
            if (length(knots) != nKnots)
            {
                # May happen if knot gaps fit exactly into both sides of the path
                flag(OL$Warning, "Didn't get the expected number of knots")
                return (NULL)
            }
        }
        else
        {
            maxKnots <- floor(lineLength / gap) + 1
            knots <- seedLoc + (-maxKnots:maxKnots * gap)
            knots <- knots[which(knots>=0 & knots<=lineLength)]
        }
        
        if (length(knots) < 2)
        {
            report(OL$Info, "Streamline is too short to fit a B-spline with at least 2 knots")
            return (NULL)
        }
        
        seedKnot <- which(knots==seedLoc)
        if (length(seedKnot) != 1)
        {
            # Should no longer happen, but warning left just in case
            flag(OL$Warning, "Seed knot is outside the line")
            return (NULL)
        }

        line <- tractor.reg::translatePoints(streamlineTract$getLine(), -streamlineTract$getSeedPoint())
        data <- data.frame(t=pointLocs, x=line[,1], y=line[,2], z=line[,3])
        knotRange <- range(knots)
        data <- subset(data, t>=knotRange[1] & t<=knotRange[2])
        ends <- c(1, length(knots))
        
        # Copy the relevant info into a clean environment to avoid baggage in "lm" objects
        workingEnvironment <- new.env(parent=globalenv())
        assign("data", data, envir=workingEnvironment)
        assign("knots", knots, envir=workingEnvironment)
        assign("ends", ends, envir=workingEnvironment)
        assign("bs", splines::bs, envir=workingEnvironment)
        
        basis <- bs(data$t, degree=3, knots=knots[-ends], Boundary.knots=knots[ends])
        
        modelX <- local(lm(x ~ bs(t,degree=3,knots=knots[-ends],Boundary.knots=knots[ends]), data=data), workingEnvironment)
        modelY <- local(lm(y ~ bs(t,degree=3,knots=knots[-ends],Boundary.knots=knots[ends]), data=data), workingEnvironment)
        modelZ <- local(lm(z ~ bs(t,degree=3,knots=knots[-ends],Boundary.knots=knots[ends]), data=data), workingEnvironment)
        models <- list(modelX, modelY, modelZ)

        knotLocsX <- as.vector(predict(modelX, data.frame(t=knots)))
        knotLocsY <- as.vector(predict(modelY, data.frame(t=knots)))
        knotLocsZ <- as.vector(predict(modelZ, data.frame(t=knots)))
        knotLocs <- matrix(c(knotLocsX, knotLocsY, knotLocsZ), ncol=3)

        return (list(basis=basis, models=models, knotPositions=knots, knotLocs=knotLocs, seedKnot=seedKnot))
    }
    
    streamlineTract$setCoordinateUnit("mm")
    
    if (is.null(knotSpacing))
    {
        report(OL$Info, "Fitting B-spline model for accuracy")
        for (nKnots in 2:100)
        {
            approximateKnotSpacing <- streamlineTract$getLineLength() / nKnots
            currentStreamline <- streamlineTract$copy()$setMaximumSpacing(approximateKnotSpacing)
            bSpline <- fitBSplineModels(currentStreamline, nKnots=nKnots)
            if (is.null(bSpline))
                next
            
            residualStandardErrors <- c(summary(bSpline$models[[1]])$sigma,
                                        summary(bSpline$models[[2]])$sigma,
                                        summary(bSpline$models[[3]])$sigma)
            meanError <- mean(residualStandardErrors)
            if (is.nan(meanError))
                report(OL$Error, "Knot spacing now too narrow - no fit possible for residual error threshold of ", signif(maxResidError,3))
            else if (meanError <= maxResidError)
            {
                knotSpacing <- diff(attr(bSpline$basis, "knots"))[1]
                report(OL$Info, "Spline with ", nKnots, " knots has mean residual error of ", signif(meanError,3))
                report(OL$Info, "Knot spacing is ", signif(knotSpacing,3))
                break
            }
        }
        
        if (is.null(knotSpacing))
            report(OL$Error, "Cannot fit a model with 100 or less knots and residual error below ", maxResidError)
    }
    else
    {
        flag(OL$Info, "Fitting B-spline model with fixed knot spacing of ", signif(knotSpacing,3))
        streamlineTract <- streamlineTract$copy()$setMaximumSpacing(knotSpacing)
        bSpline <- fitBSplineModels(streamlineTract, gap=knotSpacing)
    }
    
    if (is.null(bSpline))
        invisible (NA)
    else
    {
        bSplineTract <- BSplineTract$new(splineDegree=as.integer(attr(bSpline$basis,"degree")), splineModels=bSpline$models, knotPositions=bSpline$knotPositions, knotLocations=bSpline$knotLocs, seedKnot=as.integer(bSpline$seedKnot))
        invisible (bSplineTract)
    }
}

newBSplineTractFromStreamlineWithConstraints <- function (streamlineTract, ..., maxAngle = NULL)
{
    bSplineTract <- newBSplineTractFromStreamline(streamlineTract, ...)
    
    # Iterative spline fitting process
    repeat
    {
        if (!is(bSplineTract, "BSplineTract"))
            break
        
        leftCount <- rightCount <- 0
        
        if (!is.null(maxAngle))
        {
            steps <- characteriseSplineStepVectors(bSplineTract, "knot")

            leftSharp <- which(steps$leftAngles > maxAngle)
            rightSharp <- which(steps$rightAngles > maxAngle)

            leftStop <- ifelse(length(leftSharp) > 0, min(leftSharp), steps$leftLength+1)
            rightStop <- ifelse(length(rightSharp) > 0, min(rightSharp), steps$rightLength+1)
            leftCount <- steps$leftLength - leftStop + 1
            rightCount <- steps$rightLength - rightStop + 1
        }

        if (leftCount == 0 && rightCount == 0)
            break
        else
        {
            report(OL$Info, "Trimming ", leftCount, " left side and ", rightCount, " right side knots")
            
            spacings <- streamlineTract$getPointSpacings()
            leftSum <- cumsum(spacings)
            rightSum <- cumsum(rev(spacings))
            
            trimLeft <- leftCount * bSplineTract$getKnotSpacing()
            trimRight <- rightCount * bSplineTract$getKnotSpacing()
            leftStop <- ifelse(max(leftSum) > trimLeft, min(which(leftSum > trimLeft))+1, 1)
            rightStop <- streamlineTract$nPoints() - ifelse(max(rightSum) > trimRight, min(which(rightSum > trimRight)), 0)
            
            currentStreamline <- streamlineTract$copy()$trim(leftStop, rightStop)
            bSplineTract <- newBSplineTractFromStreamline(currentStreamline, ...)
        }
    }
    
    invisible (bSplineTract)
}

getPointsForTract <- function (tract, pointType = c("control", "knot"))
{
    if (!is(tract, "BSplineTract"))
        report(OL$Error, "The specified tract is not a valid BSplineTract object")
    
    pointType <- match.arg(pointType)
    
    if (pointType == "control")
    {
        points <- tract$getControlPoints()
        seedPoint <- tract$getSeedControlPoint()
    }
    else
    {
        points <- tract$getKnotLocations()
        seedPoint <- tract$getSeedKnot()
    }
    
    return (list(points=points, seedPoint=seedPoint))
}

calculateSplineStepVectors <- function (tract, pointType)
{
    points <- getPointsForTract(tract, pointType)
    invisible (calculateStepVectors(points$points, points$seedPoint))
}

characteriseSplineStepVectors <- function (tract, pointType)
{
    points <- getPointsForTract(tract, pointType)
    invisible (characteriseStepVectors(points$points, points$seedPoint))
}

calculateBetweenSplineAngles <- function (tract1, tract2, pointType = c("control","knot"))
{
    pointType <- match.arg(pointType)
    vectors1 <- calculateSplineStepVectors(tract1, pointType=pointType)
    vectors2 <- calculateSplineStepVectors(tract2, pointType=pointType)
    
    leftAngles <- c(NA, anglesBetweenMatrices(vectors1$left[-1,], vectors2$left[-1,]))
    rightAngles <- c(NA, anglesBetweenMatrices(vectors1$right[-1,], vectors2$right[-1,]))
    
    invisible (list(leftAngles=leftAngles, rightAngles=rightAngles))
}

calculateOffsetBetweenSplineAngles <- function (refTract, candTract, pointType = c("control","knot"))
{
    pointType <- match.arg(pointType)
    refVectors <- calculateSplineStepVectors(refTract, pointType=pointType)
    
    cand <- characteriseSplineStepVectors(candTract, pointType=pointType)
    candLeftVectors <- cand$leftVectors[-cand$leftLength,,drop=FALSE]
    candRightVectors <- cand$rightVectors[-cand$rightLength,,drop=FALSE]
    if (cand$rightLength > 2)
        candLeftVectors[1,] <- -candRightVectors[2,]
    
    leftAngles <- c(NA, anglesBetweenMatrices(refVectors$left[-1,], candLeftVectors))
    rightAngles <- c(NA, NA, anglesBetweenMatrices(refVectors$right[-(1:2),], candRightVectors[-1,]))
    
    invisible (list(leftAngles=leftAngles, rightAngles=rightAngles))
}
