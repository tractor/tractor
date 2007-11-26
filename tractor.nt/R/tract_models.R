.UninformativeTractModel <- function (.lengthDistributions, .refLengths, .pointType)
{
    self <- list(
        # The selection of the left distribution here is arbitrary - it is
        # assumed that the length cutoff is the same on both sides
        getMaximumLength = function () { return (max(.lengthDistributions$left$values)) },
        
        getPointType = function () { return (.pointType) },
        
        getLeftLengthDistribution = function () { return (.lengthDistributions$left) },
        
        getRightLengthDistribution = function () { return (.lengthDistributions$right) },
        
        getRefLeftLength = function () { return (.refLengths$left) },
        
        getRefRightLength = function () { return (.refLengths$right) },
        
        summarise = function ()
        {
            output(OL$Info, "Ref tract lengths : ", self$getRefLeftLength(), " (left), ", self$getRefRightLength(), " (right)")
            output(OL$Info, "Length cutoff     : ", self$getMaximumLength())
            output(OL$Info, "Point type        : ", self$getPointType())
        },

        summarize = function () { self$summarise() }
    )
    
    class(self) <- c("model.tract.uninformative", "list.object", "list")
    invisible (self)
}

.NonmatchingTractModel <- function (.cosineDistribution, .lengthDistributions, .pointType)
{
    self <- list(
        getCosineDistribution = function () { return (.cosineDistribution) },
        
        getPointType = function () { return (.pointType) },
        
        getLeftLengthDistribution = function () { return (.lengthDistributions$left) },
        
        getRightLengthDistribution = function () { return (.lengthDistributions$right) },
        
        summarise = function ()
        {
            output(OL$Info, "Alpha      : ", round(self$getCosineDistribution()$betaFit$alpha, 2))
            output(OL$Info, "Epsilon    : ", signif(self$getCosineDistribution()$epsilon, 2))
            output(OL$Info, "Point type : ", self$getPointType())
        },
        
        summarize = function () { self$summarise() }
    )
    
    class(self) <- c("model.tract.nonmatching", "list.object", "list")
    invisible (self)
}

.MatchingTractModel <- function (.cosineDistributions, .lengthDistributions, .refSpline, .pointType)
{
    if (!isBSplineTract(.refSpline))
        output(OL$Error, "The specified reference tract is not a BSplineTract object")
    
    ssv <- characteriseSplineStepVectors(.refSpline, pointType=.pointType)
    .refLengths <- list(left=ssv$leftLength, right=ssv$rightLength)
    rm(ssv)
    
    self <- list(
        getAlphas = function ()
        {
            alphas <- numeric(0)
            for (i in 1:length(.cosineDistributions))
                alphas <- c(alphas, as.list(.cosineDistributions[[i]])$alpha)
            return (alphas)
        },
        
        getCosineDistribution = function (pos)
        {
            if ((pos < 1) || (pos > length(.cosineDistributions)))
                return (NA)
            else
                return (.cosineDistributions[[pos]])
        },
        
        getMaximumLength = function () { return (max(.lengthDistributions$left$values)) },
        
        getPointType = function () { return (.pointType) },
        
        getLeftLengthDistribution = function () { return (.lengthDistributions$left) },
        
        getRightLengthDistribution = function () { return (.lengthDistributions$right) },
        
        getRefLeftLength = function () { return (.refLengths$left) },
        
        getRefRightLength = function () { return (.refLengths$right) },
        
        getRefSpline = function () { return (.refSpline) },
        
        summarise = function ()
        {
            output(OL$Info, "Alphas            : ", implode(round(self$getAlphas(),2), sep=", "))
            output(OL$Info, "Ref tract lengths : ", self$getRefLeftLength(), " (left), ", self$getRefRightLength(), " (right)")
            output(OL$Info, "Length cutoff     : ", self$getMaximumLength())
            output(OL$Info, "Point type        : ", self$getPointType())
        },
        
        summarize = function () { self$summarise() }
    )
    
    class(self) <- c("model.tract.matching", "list.object", "list")
    invisible (self)
}

isUninformativeTractModel <- function (object)
{
    return ("model.tract.uninformative" %in% class(object))
}

isNonmatchingTractModel <- function (object)
{
    return ("model.tract.nonmatching" %in% class(object))
}

isMatchingTractModel <- function (object)
{
    return ("model.tract.matching" %in% class(object))
}

calculateRescaledCosinesFromAngles <- function (angles, naRemove = TRUE)
{
    if (naRemove)
        angles <- angles[!is.na(angles)]
    cosines <- 0.5 * (cos(angles)+1)
    return (cosines)
}

createDataTableForSplines <- function (splines, refSpline, pointType, subjectId = NULL)
{
    if (!is.list(splines))
        output(OL$Error, "Spline tracts must be specified as a list of BSplineTract objects")
    if (!isBSplineTract(refSpline))
        output(OL$Error, "The specified reference tract is not a BSplineTract object")
    
    nSplines <- length(splines)
    rssv <- characteriseSplineStepVectors(refSpline, pointType=pointType)
    
    leftLengths <- rep(NA, nSplines)
    rightLengths <- rep(NA, nSplines)
    similarityCosines <- matrix(NA, nrow=nSplines, ncol=(rssv$leftLength+rssv$rightLength))
    colnames(similarityCosines) <- c(paste("leftSimCosine",1:rssv$leftLength,sep=""), paste("rightSimCosine",1:rssv$rightLength,sep=""))
    
    for (i in seq_along(splines))
    {
        if (isTRUE(is.na(splines[[i]])))
            next

        ssv <- characteriseSplineStepVectors(splines[[i]], pointType=pointType)
        leftLengths[i] <- ssv$leftLength
        rightLengths[i] <- ssv$rightLength
        bsa <- calculateBetweenSplineAngles(refSpline, splines[[i]], pointType=pointType)
        leftIndices <- 1:min(length(bsa$leftAngles),rssv$leftLength)
        rightIndices <- 1:min(length(bsa$rightAngles),rssv$rightLength) + rssv$leftLength
        similarityCosines[i,leftIndices] <- calculateRescaledCosinesFromAngles(bsa$leftAngles[1:length(leftIndices)], naRemove=FALSE)
        similarityCosines[i,rightIndices] <- calculateRescaledCosinesFromAngles(bsa$rightAngles[1:length(rightIndices)], naRemove=FALSE)
    }

    data <- data.frame(pointType=rep(pointType,nSplines), leftLength=leftLengths, rightLength=rightLengths)
    if (!is.null(subjectId))
        data <- cbind(data, data.frame(subject=rep(subjectId,nSplines)))
    data <- cbind(data, similarityCosines)
    
    invisible (data)
}

newUninformativeTractModelFromDataTable <- function (data, maxLength, weights = NULL)
{
    if (!is.null(weights))
    {
        if (length(weights) != nrow(data))
            output(OL$Error, "The weight vector must have the same length as the spline data table")
        
        weights[is.na(data$leftLength)] <- NA       
    }
    
    leftLengthDistribution <- fitMultinomialDistribution(data$leftLength, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(data$rightLength, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)

    refLeftLength <- length(grep("^leftSimCosine", colnames(data)))
    refRightLength <- length(grep("^rightSimCosine", colnames(data)))
    refLengths <- list(left=refLeftLength, right=refRightLength)

    model <- .UninformativeTractModel(lengthDistributions, refLengths, as.character(data$pointType[1]))
    invisible (model)
}

newMatchingTractModelFromDataTable <- function (data, refSpline, maxLength, lambda = NULL, weights = NULL)
{
    if (is.null(weights))
        weights <- rep(1,nrow(data))
    else if (length(weights) != nrow(data))
        output(OL$Error, "The weight vector must have the same length as the spline data table")
    weights[is.na(data$leftLength)] <- NA
    
    leftLengthDistribution <- fitMultinomialDistribution(data$leftLength, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(data$rightLength, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
    refLeftLength <- length(grep("^leftSimCosine", colnames(data)))
    refRightLength <- length(grep("^rightSimCosine", colnames(data)))
    
    cosineDistributions <- list()
    for (i in 1:max(refLeftLength,refRightLength))
    {
        cosines <- c(data[[paste("leftSimCosine",i,sep="")]], data[[paste("rightSimCosine",i,sep="")]])
        cosineWeights <- rep(weights, length(cosines)/length(weights))
        cosineWeights[is.na(cosines)] <- NA
        
        if (is.null(cosines) || sum(!is.na(cosines)) == 0)
            cosineDistributions <- c(cosineDistributions, NA)
        else
            cosineDistributions <- c(cosineDistributions, list(fitRegularisedBetaDistribution(cosines, lambda=lambda, weights=cosineWeights)))
    }
    
    model <- .MatchingTractModel(cosineDistributions, lengthDistributions, refSpline, as.character(data$pointType[1]))
    invisible (model)
}

newUninformativeTractModelFromSplines <- function (refSpline, nonmatchingSplines, maxLength, pointType, weights = NULL)
{
    if (!isBSplineTract(refSpline))
        output(OL$Error, "The specified reference tract is not a BSplineTract object")
    if (!is.list(nonmatchingSplines) || !isBSplineTract(nonmatchingSplines[[1]]))
        output(OL$Error, "The nonmatching tracts must be specified as a list of BSplineTract objects")
    if (!is.null(weights) && (length(weights) != length(nonmatchingSplines)))
        output(OL$Error, "The weight vector must have the same length as the list of nonmatching splines")

    leftLengths <- numeric(0)
    rightLengths <- numeric(0)
    for (i in 1:length(nonmatchingSplines))
    {
        if (is.na(nonmatchingSplines[[i]]))
            next
        
        ssv <- characteriseSplineStepVectors(nonmatchingSplines[[i]], pointType=pointType)
        leftLengths <- c(leftLengths, ssv$leftLength)
        rightLengths <- c(rightLengths, ssv$rightLength)
    }

    leftLengthDistribution <- fitMultinomialDistribution(leftLengths, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(rightLengths, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
    ssv <- characteriseSplineStepVectors(refSpline, pointType=pointType)
    refLengths <- list(left=ssv$leftLength, right=ssv$rightLength)
    
    model <- .UninformativeTractModel(lengthDistributions, refLengths, pointType)
    invisible (model)
}

newNonmatchingTractModelFromSplines <- function (nonmatchingSplines, maxLength, pointType)
{
    if (!is.list(nonmatchingSplines) || !isBSplineTract(nonmatchingSplines[[1]]))
        output(OL$Error, "The nonmatching tracts must be specified as a list of BSplineTract objects")
    
    angles <- numeric(0)
    leftLengths <- numeric(0)
    rightLengths <- numeric(0)
    for (i in 1:length(nonmatchingSplines))
    {
        if (is.na(nonmatchingSplines[[i]]))
            next
        
        ssv <- characteriseSplineStepVectors(nonmatchingSplines[[i]], pointType=pointType)
        leftLengths <- c(leftLengths, ssv$leftLength)
        rightLengths <- c(rightLengths, ssv$rightLength)
        angles <- c(angles, ssv$leftAngles, ssv$rightAngles, ssv$middleAngle)
    }
    
    leftLengthDistribution <- fitMultinomialDistribution(leftLengths, const=1, values=0:maxLength)
    rightLengthDistribution <- fitMultinomialDistribution(rightLengths, const=1, values=0:maxLength)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
    cosines <- calculateRescaledCosinesFromAngles(angles)
    cosineDistribution <- fitCosineDistribution(cosines)
    
    model <- .NonmatchingTractModel(cosineDistribution, lengthDistributions, pointType)
    invisible (model)
}

newMatchingTractModelFromSplines <- function (refSpline, matchingSplines, maxLength, pointType, weights = NULL)
{
    if (!isBSplineTract(refSpline))
        output(OL$Error, "The specified reference tract is not a BSplineTract object")
    if (!is.list(matchingSplines) || !isBSplineTract(matchingSplines[[1]]))
        output(OL$Error, "The matching tracts must be specified as a list of BSplineTract objects")
    
    if (is.null(weights))
        weights <- rep(1, length(matchingSplines))
    else if (length(weights) != length(matchingSplines))
        output(OL$Error, "The weight vector must have the same length as the list of matching splines")
    
    angles <- numeric(0)
    positions <- numeric(0)
    sourceWeights <- numeric(0)
    leftLengths <- numeric(0)
    rightLengths <- numeric(0)
    for (i in 1:length(matchingSplines))
    {
        if (is.na(matchingSplines[[i]]))
            next
        
        ssv <- characteriseSplineStepVectors(matchingSplines[[i]], pointType=pointType)
        leftLengths <- c(leftLengths, ssv$leftLength)
        rightLengths <- c(rightLengths, ssv$rightLength)
        bsa <- calculateBetweenSplineAngles(refSpline, matchingSplines[[i]], pointType=pointType)
        angles <- c(angles, bsa$leftAngles, bsa$rightAngles)
        sourceWeights <- c(sourceWeights, rep(weights[i],length(c(bsa$leftAngles,bsa$rightAngles))))
        positions <- c(positions, seq_along(bsa$leftAngles), seq_along(bsa$rightAngles))
    }
    
    leftLengthDistribution <- fitMultinomialDistribution(leftLengths, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(rightLengths, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
    cosineDistributions <- list()
    angleList <- tapply(angles, factor(positions), "c")
    weightList <- tapply(sourceWeights, factor(positions), "c")
    for (i in 1:max(positions))
    {
        currentAngles <- angleList[[as.character(i)]]
        currentWeights <- weightList[[as.character(i)]]
        cosines <- calculateRescaledCosinesFromAngles(currentAngles)
        cosineWeights <- currentWeights[!is.na(currentAngles)]
        if (is.null(cosines) || length(cosines) == 0)
            cosineDistributions <- c(cosineDistributions, NA)
        else
            cosineDistributions <- c(cosineDistributions, list(fitBetaDistribution(cosines, weights=cosineWeights)))
    }
    
    model <- .MatchingTractModel(cosineDistributions, lengthDistributions, refSpline, pointType)
    invisible (model)
}

fitCosineDistribution <- function (cosines, epsilon = 1e-5)
{
    betaFit <- fitBetaDistribution(cosines, alpha=NULL, beta=1)
    previousEpsilon <- epsilon
    
    repeat
    {
        betaResps <- (1-epsilon) * evaluateBetaDistribution(cosines, betaFit)
        unifResps <- rep(epsilon, length(cosines))
        betaWeights <- betaResps / (betaResps + unifResps)
        unifWeights <- unifResps / (betaResps + unifResps)
        
        betaFit$alpha <- sum(-betaWeights) / sum(betaWeights * log(cosines))
        epsilon <- sum(unifWeights) / sum(betaWeights + unifWeights)
        
        output(OL$Debug, "Alpha = ", betaFit$alpha, "; epsilon = ", epsilon)
        
        if (epsilon == 0)
            break
        else if (abs(log(previousEpsilon/epsilon)) < 0.1)
            break
        else
            previousEpsilon <- epsilon
    }
    
    fullDistribution <- list(betaFit=betaFit, epsilon=epsilon)
    class(fullDistribution) <- c("distribution.mixed.cosine", "list")
    return (fullDistribution)
}

evaluateCosineDistribution <- function (x, params, log = FALSE)
{
    if (!is.list(params))
        return (NA)
    if (!is.numeric(x) || length(x) == 0)
        return (NA)
        
    betaResult <- evaluateBetaDistribution(x, params$betaFit, log=FALSE)
    finalResult <- params$epsilon + ((1-params$epsilon) * betaResult)
    log <- rep(log, length(finalResult))
    
    return (ifelse(log==TRUE, log(finalResult), finalResult))
}

calculateUninformativeLogLikelihoodsForDataTable <- function (data, uninformativeModel)
{
    if (!isUninformativeTractModel(uninformativeModel))
        output(OL$Error, "The specified model is not an UninformativeTractModel object")
    
    # The evaluateMultinomialDistribution function is not vectorised at present
    lls <- numeric(nrow(data))
    for (i in 1:nrow(data))
        lls[i] <- evaluateMultinomialDistribution(data$leftLength[i], uninformativeModel$getLeftLengthDistribution(), log=TRUE) + evaluateMultinomialDistribution(data$rightLength[i], uninformativeModel$getRightLengthDistribution(), log=TRUE)
    
    shorterLeftLengths <- pmin(data$leftLength, uninformativeModel$getRefLeftLength())
    shorterRightLengths <- pmin(data$rightLength, uninformativeModel$getRefRightLength())
    lls <- lls + (shorterLeftLengths + shorterRightLengths) * log(0.5)
    
    return (lls)
}

calculateMatchedLogLikelihoodsForDataTable <- function (data, matchingModel)
{
    if (!isMatchingTractModel(matchingModel))
        output(OL$Error, "The specified model is not a MatchingTractModel object")
    
    lls <- numeric(nrow(data))
    for (i in 1:nrow(data))
        lls[i] <- evaluateMultinomialDistribution(data$leftLength[i], matchingModel$getLeftLengthDistribution(), log=TRUE) + evaluateMultinomialDistribution(data$rightLength[i], matchingModel$getRightLengthDistribution(), log=TRUE)
    
    for (j in 2:matchingModel$getRefLeftLength())
    {
        contribs <- evaluateBetaDistribution(data[[paste("leftSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j), log=TRUE)
        lls <- lls + replace(contribs, is.na(contribs), 0)
    }
    for (j in 2:matchingModel$getRefRightLength())
    {
        contribs <- evaluateBetaDistribution(data[[paste("rightSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j), log=TRUE)
        lls <- lls + replace(contribs, is.na(contribs), 0)
    }
    
    return (lls)
}

calculateUninformativeLogLikelihoodForSpline <- function (spline, uninformativeModel)
{
    if (!isUninformativeTractModel(uninformativeModel))
        output(OL$Error, "The specified model is not an UninformativeTractModel object")
    
    ssv <- characteriseSplineStepVectors(spline, pointType=uninformativeModel$getPointType())
    shorterLeftLength <- min(ssv$leftLength, uninformativeModel$getRefLeftLength())
    shorterRightLength <- min(ssv$rightLength, uninformativeModel$getRefRightLength())
    
    ll <- evaluateMultinomialDistribution(ssv$leftLength, uninformativeModel$getLeftLengthDistribution(), log=TRUE) + evaluateMultinomialDistribution(ssv$rightLength, uninformativeModel$getRightLengthDistribution(), log=TRUE)
    ll <- ll + (shorterLeftLength + shorterRightLength) * log(0.5)
    
    return (ll)
}

calculateNonmatchedLogLikelihoodForSpline <- function (spline, nonmatchingModel)
{
    if (!isNonmatchingTractModel(nonmatchingModel))
        output(OL$Error, "The specified model is not a NonmatchingTractModel object")
    
    ssv <- characteriseSplineStepVectors(spline, pointType=nonmatchingModel$getPointType())
    
    ll <- evaluateMultinomialDistribution(ssv$leftLength, nonmatchingModel$getLeftLengthDistribution(), log=TRUE) + evaluateMultinomialDistribution(ssv$rightLength, nonmatchingModel$getRightLengthDistribution(), log=TRUE)
    
    angles <- c(ssv$leftAngles, ssv$rightAngles, ssv$middleAngle)
    cosines <- calculateRescaledCosinesFromAngles(angles)
    ll <- ll + sum(evaluateCosineDistribution(cosines, nonmatchingModel$getCosineDistribution(), log=TRUE))
    
    return (ll)
}

calculateMatchedLogLikelihoodForSpline <- function (spline, matchingModel)
{
    if (!isMatchingTractModel(matchingModel))
        output(OL$Error, "The specified model is not a MatchingTractModel object")
    
    ssv <- characteriseSplineStepVectors(spline, pointType=matchingModel$getPointType())
    bsa <- calculateBetweenSplineAngles(matchingModel$getRefSpline(), spline, pointType=matchingModel$getPointType())
    
    shorterLeftLength <- min(ssv$leftLength, matchingModel$getRefLeftLength())
    shorterRightLength <- min(ssv$rightLength, matchingModel$getRefRightLength())
    
    ll <- evaluateMultinomialDistribution(ssv$leftLength, matchingModel$getLeftLengthDistribution(), log=TRUE) + evaluateMultinomialDistribution(ssv$rightLength, matchingModel$getRightLengthDistribution(), log=TRUE)
    
    if (shorterLeftLength > 1)
    {
        for (i in 2:shorterLeftLength)
        {
            cosine <- calculateRescaledCosinesFromAngles(bsa$leftAngles[i])
            ll <- ll + evaluateBetaDistribution(cosine, matchingModel$getCosineDistribution(i), log=TRUE)
        }
    }
    if (shorterRightLength > 1)
    {
        for (i in 2:shorterRightLength)
        {
            cosine <- calculateRescaledCosinesFromAngles(bsa$rightAngles[i])
            ll <- ll + evaluateBetaDistribution(cosine, matchingModel$getCosineDistribution(i), log=TRUE)
        }
    }
    
    return (ll)
}
