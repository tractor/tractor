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

isMatchingTractModel <- function (object)
{
    return ("model.tract.matching" %in% class(object))
}

deserialiseMatchingTractModel <- function (file = NULL, object = NULL)
{
    model <- deserialiseListObject(file, object, .MatchingTractModel)
    invisible (model)
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

newUninformativeTractModelFromDataTable <- function (data, maxLength = NULL, weights = NULL)
{
    if (!is.null(weights))
    {
        if (length(weights) != nrow(data))
            output(OL$Error, "The weight vector must have the same length as the spline data table")
        
        weights[is.na(data$leftLength)] <- NA       
    }
    
    if (is.null(maxLength))
        maxLength <- max(data$leftLength, data$rightLength, na.rm=TRUE)
    
    leftLengthDistribution <- fitMultinomialDistribution(data$leftLength, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(data$rightLength, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)

    refLeftLength <- length(grep("^leftSimCosine", colnames(data)))
    refRightLength <- length(grep("^rightSimCosine", colnames(data)))
    refLengths <- list(left=refLeftLength, right=refRightLength)

    model <- .UninformativeTractModel(lengthDistributions, refLengths, as.character(data$pointType[1]))
    invisible (model)
}

newMatchingTractModelFromDataTable <- function (data, refSpline, maxLength = NULL, lambda = NULL, weights = NULL)
{
    if (is.null(weights))
        weights <- rep(1,nrow(data))
    else if (length(weights) != nrow(data))
        output(OL$Error, "The weight vector must have the same length as the spline data table")
    weights[is.na(data$leftLength)] <- NA
    
    refLeftLength <- length(grep("^leftSimCosine", colnames(data)))
    refRightLength <- length(grep("^rightSimCosine", colnames(data)))
    
    if (is.null(maxLength))
        maxLength <- max(data$leftLength, data$rightLength, refLeftLength, refRightLength, na.rm=TRUE)
    
    leftLengthDistribution <- fitMultinomialDistribution(data$leftLength, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(data$rightLength, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
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
        # Implicitly use a uniform distribution (i.e. fixed log likelihood
        # contribution of 0) if the model is not trained at this point
        if (is.list(matchingModel$getCosineDistribution(j)))
        {
            contribs <- evaluateBetaDistribution(data[[paste("leftSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    for (j in 2:matchingModel$getRefRightLength())
    {
        if (is.list(matchingModel$getCosineDistribution(j)))
        {
            contribs <- evaluateBetaDistribution(data[[paste("rightSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    
    return (lls)
}
