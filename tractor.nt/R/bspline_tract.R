.BSplineTract <- function (.splineBasis, .splineModels, .knotLocations, .seedKnot)
{
    .degree <- attr(.splineBasis, "degree")
    .nKnots <- length(attr(.splineBasis, "knots"))
    .knotSpacings <- diff(attr(.splineBasis, "knots"))
    
    # This is a uniform B-spline, so knots should be equally spaced
    if (!equivalent(.knotSpacings, rep(.knotSpacings[1],length(.knotSpacings))))
        output(OL$Error, "Knots are not equally spaced")
    
    self <- list(
        getControlPoints = function ()
        {
            controlPoints <- array(NA, dim=c(.nKnots+.degree,3))
            indices <- 2:(.nKnots+.degree+1)
            controlPoints[,1] <- .splineModels[[1]]$coefficients[indices] + .splineModels[[1]]$coefficients[1]
            controlPoints[,2] <- .splineModels[[2]]$coefficients[indices] + .splineModels[[2]]$coefficients[1]
            controlPoints[,3] <- .splineModels[[3]]$coefficients[indices] + .splineModels[[3]]$coefficients[1]
            return (controlPoints)
        },
        
        getKnotLocations = function () { return (.knotLocations) },
        
        getKnotPositions = function () { return (attr(.splineBasis, "knots")) },
        
        getKnotSpacing = function () { return (.knotSpacings[1]) },
        
        getLineAtPoints = function (tValues)
        {
            locsX <- as.vector(predict(.splineModels[[1]], data.frame(t=tValues)))
            locsY <- as.vector(predict(.splineModels[[2]], data.frame(t=tValues)))
            locsZ <- as.vector(predict(.splineModels[[3]], data.frame(t=tValues)))
            return (matrix(c(locsX, locsY, locsZ), ncol=3))
        },
        
        getSeedControlPoint = function () { return (.seedKnot+1) },
        
        getSeedKnot = function () { return (.seedKnot) },
        
        nControlPoints = function () { return (.nKnots+.degree) },
        
        nKnots = function () { return (.nKnots) }
    )
    
    class(self) <- c("tract.bspline", "list.object", "list")
    invisible (self)
}

isBSplineTract <- function (object)
{
    return ("tract.bspline" %in% class(object))
}

deserialiseBSplineTract <- function (file = NULL, object = NULL)
{
    tract <- deserialiseListObject(file, object, .BSplineTract)
    invisible (tract)
}

newBSplineTractFromStreamline <- function (streamlineTract, knotSpacing = NULL, maxResidError = 0.1)
{
    fitBSplineModels <- function (streamlineTract, nKnots)
    {
        # All of the following numbers are parametric, in mm along the line
        lineLength <- streamlineTract$getLineLength()
        pointLocs <- cumsum(streamlineTract$getPointSpacings())
        pointLocs <- c(1, pointLocs+1)
        seedLoc <- pointLocs[streamlineTract$getSeedIndex()]

        gap <- (lineLength-1) / nKnots
        knots <- seedLoc + (-nKnots:nKnots * gap)
        knots <- knots[which(knots>1 & knots<lineLength)]
        if (length(knots) != nKnots)
            output(OL$Error, "Didn't get the expected number of knots")

        line <- streamlineTract$getLine()
        data <- data.frame(t=pointLocs, x=line[,1], y=line[,2], z=line[,3])

        basis <- bs(data$t, knots=knots, degree=3)
        modelX <- lm(x ~ bs(t,knots=knots,degree=3), data=data)
        modelY <- lm(y ~ bs(t,knots=knots,degree=3), data=data)
        modelZ <- lm(z ~ bs(t,knots=knots,degree=3), data=data)
        models <- list(modelX, modelY, modelZ)

        knotLocsX <- as.vector(predict(modelX, data.frame(t=knots)))
        knotLocsY <- as.vector(predict(modelY, data.frame(t=knots)))
        knotLocsZ <- as.vector(predict(modelZ, data.frame(t=knots)))
        knotLocs <- matrix(c(knotLocsX, knotLocsY, knotLocsZ), ncol=3)

        return (list(basis=basis, models=models, knotLocs=knotLocs, seedKnot=which(knots==seedLoc)))
    }
    
    if (is.null(knotSpacing))
    {
        output(OL$Info, "Fitting B-spline model for accuracy")
        for (nKnots in 1:100)
        {
            knotSpacing <- streamlineTract$getLineLength() / nKnots
            currentStreamline <- newStreamlineTractWithSpacingThreshold(streamlineTract, knotSpacing)
            bSpline <- fitBSplineModels(currentStreamline, nKnots)
            residualStandardErrors <- c(summary(bSpline$models[[1]])$sigma,
                                        summary(bSpline$models[[2]])$sigma,
                                        summary(bSpline$models[[3]])$sigma)
            meanError <- mean(residualStandardErrors)
            if (is.nan(meanError))
                output(OL$Error, "Knot spacing now too narrow - no fit possible for residual error threshold of ", signif(maxResidError,3))
            else if (meanError <= maxResidError)
            {
                knotSpacing <- diff(attr(bSpline$basis, "knots"))[1]
                output(OL$Info, "Spline with ", nKnots, " knots has mean residual error of ", signif(meanError,3))
                output(OL$Info, "Knot spacing is ", signif(knotSpacing,3))
                break
            }
        }
        
        if (is.null(knotSpacing))
            output(OL$Error, "Cannot fit a model with 100 or less knots and residual error below ", maxResidError)
    }
    else
    {
        output(OL$Info, "Fitting B-spline model with fixed knot spacing of ", signif(knotSpacing,3))
        streamlineTract <- newStreamlineTractWithSpacingThreshold(streamlineTract, knotSpacing)
        nKnots <- floor(streamlineTract$getLineLength() / knotSpacing)
        bSpline <- fitBSplineModels(streamlineTract, nKnots)
    }
    
    bSplineTract <- .BSplineTract(bSpline$basis, bSpline$models, bSpline$knotLocs, bSpline$seedKnot)
    invisible (bSplineTract)
}

getPointsForTract <- function (tract, pointType = c("control", "knot"))
{
    if (!isBSplineTract(tract))
        output(OL$Error, "The specified tract is not a valid BSplineTract object")
    
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
