#' The BSplineTract class
#'
#' This class represents a tract, generally derived from a single
#' tractography streamline, as a piecewise cubic B-spline curve fitted
#' independently to its x, y and z coordinates as a function of distance
#' along the streamline. Knots are equally spaced and indexed relative to a
#' designated seed knot, allowing corresponding points on different tracts to
#' be compared directly. Objects of this class are usually created by
#' [newBSplineTractFromStreamline()] or
#' [newBSplineTractFromStreamlineWithConstraints()], rather than directly.
#'
#' @field splineDegree An integer giving the degree of the B-spline basis
#'   used to fit each coordinate (in practice this is currently always 3,
#'   i.e. cubic).
#' @field splineModels A list of three fitted `lm` objects, giving the
#'   B-spline models for the x, y and z coordinates respectively, each as a
#'   function of arc-length distance along the tract.
#' @field knotPositions A numeric vector of the arc-length positions (in mm)
#'   of the spline knots, relative to the start of the tract.
#' @field knotLocations A matrix with one row per knot and three columns,
#'   giving the fitted 3D location of each knot.
#' @field seedKnot An integer giving the index, within `knotPositions` and
#'   `knotLocations`, of the knot closest to the tract's seed point.
#'
#' @export
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
        "Compute the B-spline control points from the fitted coordinate models"
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
        "Evaluate the fitted B-spline curve at a set of arc-length positions"
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

#' @export
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

#' Fit a B-spline tract to a streamline
#'
#' This function fits a smooth B-spline curve to the coordinates of a
#' candidate streamline, creating a [BSplineTract] representation which can
#' be compared, point by point, to other tracts fitted with the same knot
#' spacing. Knots are placed symmetrically about the streamline's seed point.
#'
#' If `knotSpacing` is not specified, the function iteratively increases the
#' number of knots (and hence reduces their spacing) until the mean residual
#' standard error across the three fitted coordinate models falls to or below
#' `maxResidError`, up to a maximum of 100 knots.
#'
#' @param streamlineTract A [tractor.track::Streamline] object representing a
#'   single candidate tract, with an identified seed point.
#' @param knotSpacing A number giving the spacing between knots, in mm. The
#'   default, `NULL`, causes a suitable spacing to be chosen automatically,
#'   as described above.
#' @param maxResidError A number giving the maximum acceptable mean residual
#'   standard error, across the three coordinate models, when `knotSpacing`
#'   is not specified.
#' @return A [BSplineTract] object representing the fitted spline. If no
#'   adequate fit could be obtained, `NA` is returned instead. Either way the
#'   result is returned invisibly.
#' @author Jon Clayden
#' @export
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
        
        line <- t(apply(streamlineTract$getLine(), 1, "-", streamlineTract$getSeedPoint()))
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

#' Fit a B-spline tract with removal of aberrant end sections
#'
#' This function extends [newBSplineTractFromStreamline()] by iteratively
#' trimming knots from either end of the fitted spline where the tract turns
#' more sharply than `maxAngle` allows, and refitting, until no more knots
#' need to be removed. This is useful for removing aberrant sections of
#' tractography streamlines distal to the seed point, which can otherwise
#' adversely affect matching to a reference tract.
#'
#' @param streamlineTract A [tractor.track::Streamline] object representing a
#'   single candidate tract, with an identified seed point.
#' @param ... Additional arguments to [newBSplineTractFromStreamline()], most
#'   usually `knotSpacing`.
#' @param maxAngle A number giving the maximum acceptable angle, in radians,
#'   between consecutive knot-to-knot step vectors on either side of the seed
#'   knot. Knots beyond the first point at which this threshold is exceeded
#'   are trimmed from the streamline before refitting. The default, `NULL`,
#'   disables trimming.
#' @return A [BSplineTract] object representing the fitted (and possibly
#'   trimmed) spline, or `NA` if no fit could be obtained. The result is
#'   returned invisibly.
#' @author Jon Clayden
#' @export
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

#' Extract representative points from a B-spline tract
#'
#' This function extracts either the control points or the knot locations of
#' a [BSplineTract], along with the index of its seed point, in the common
#' form used by [calculateSplineStepVectors()] and related functions.
#'
#' @param tract A [BSplineTract] object.
#' @param pointType A string, either `"control"` (the default) or `"knot"`,
#'   indicating whether the spline's control points or its fitted knot
#'   locations should be returned.
#' @return A list with elements `points`, a matrix of 3D point coordinates,
#'   and `seedPoint`, the index of the seed point within `points`.
#' @author Jon Clayden
#' @export
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

#' Step vectors along a B-spline tract
#'
#' These functions calculate the vectors between successive representative
#' points of a [BSplineTract], working outwards from its seed point in each
#' direction, and (for `characteriseSplineStepVectors`) some derived
#' properties of these step vectors. They are thin wrappers around
#' [calculateStepVectors()] and [characteriseStepVectors()], which first
#' extract suitable points from the tract using [getPointsForTract()].
#'
#' @param tract A [BSplineTract] object.
#' @param pointType A string, either `"control"` or `"knot"`, passed to
#'   [getPointsForTract()].
#' @return For `calculateSplineStepVectors`, a list with elements `left` and
#'   `right`, each a matrix of step vectors moving outwards from the seed
#'   point in the corresponding direction. For `characteriseSplineStepVectors`
#'   see [characteriseStepVectors()] for the additional derived elements
#'   returned.
#' @author Jon Clayden
#' @export
calculateSplineStepVectors <- function (tract, pointType)
{
    points <- getPointsForTract(tract, pointType)
    invisible (calculateStepVectors(points$points, points$seedPoint))
}

#' @rdname calculateSplineStepVectors
#' @export
characteriseSplineStepVectors <- function (tract, pointType)
{
    points <- getPointsForTract(tract, pointType)
    invisible (characteriseStepVectors(points$points, points$seedPoint))
}

#' Angles between corresponding points on two B-spline tracts
#'
#' This function compares two [BSplineTract] objects, indexed from their own
#' seed knots, by calculating the angle between each pair of corresponding
#' step vectors on the left and right sides of the seed.
#'
#' @param tract1, tract2 [BSplineTract] objects to compare.
#' @param pointType A string, either `"control"` or `"knot"`, passed to
#'   [getPointsForTract()].
#' @return A list with elements `leftAngles` and `rightAngles`, giving the
#'   angle (in radians) between the corresponding step vectors of the two
#'   tracts at each position from the seed outwards. The first element of
#'   each is always `NA`, since there is no step vector at the seed itself.
#' @author Jon Clayden
#' @export
calculateBetweenSplineAngles <- function (tract1, tract2, pointType = c("control","knot"))
{
    pointType <- match.arg(pointType)
    vectors1 <- calculateSplineStepVectors(tract1, pointType=pointType)
    vectors2 <- calculateSplineStepVectors(tract2, pointType=pointType)
    
    leftAngles <- c(NA, anglesBetweenMatrices(vectors1$left[-1,], vectors2$left[-1,]))
    rightAngles <- c(NA, anglesBetweenMatrices(vectors1$right[-1,], vectors2$right[-1,]))
    
    invisible (list(leftAngles=leftAngles, rightAngles=rightAngles))
}

#' Compare two B-spline tracts allowing for a knot offset
#'
#' This function is similar to [calculateBetweenSplineAngles()], but allows
#' for the possibility that the candidate tract's step vectors are offset by
#' one position relative to the reference tract's, which can occur if the
#' underlying streamlines were resampled to slightly different point
#' separations before fitting.
#'
#' @param refTract A [BSplineTract] object representing the reference tract.
#' @param candTract A [BSplineTract] object representing the candidate tract
#'   to compare against the reference.
#' @param pointType A string, either `"control"` or `"knot"`, passed to
#'   [getPointsForTract()].
#' @return A list with elements `leftAngles` and `rightAngles`, as for
#'   [calculateBetweenSplineAngles()].
#' @author Jon Clayden
#' @export
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
