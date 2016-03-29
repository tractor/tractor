UninformativeTractModel <- setRefClass("UninformativeTractModel", contains="SerialisableObject", fields=list(lengthDistributions="list",refLengths="list",pointType="character"), methods=list(
    # The selection of the left distribution here is arbitrary - it is
    # assumed that the length cutoff is the same on both sides
    getMaximumLength = function () { return (max(lengthDistributions$left$values)) },
    
    getPointType = function () { return (pointType) },
    
    getLeftLengthDistribution = function () { return (lengthDistributions$left) },
    
    getRightLengthDistribution = function () { return (lengthDistributions$right) },
    
    getRefLeftLength = function () { return (refLengths$left) },
    
    getRefRightLength = function () { return (refLengths$right) },
    
    summarise = function ()
    {
        labels <- c("Ref tract lengths", "Length cutoff", "Point type")
        values <- c(paste(getRefLeftLength(), " (left), ", getRefRightLength(), " (right)",sep=""), getMaximumLength(), getPointType())
        return (list(labels=labels, values=values))
    }
))

MatchingTractModel <- setRefClass("MatchingTractModel", contains="SerialisableObject", fields=list(cosineDistributions="list",lengthDistributions="list",refSpline="BSplineTract",refLengths="list",pointType="character"), methods=list(
    initialize = function (..., refSpline = nilObject())
    {
        object <- initFields(..., refSpline=as(refSpline,"BSplineTract"))
        
        if (!is.nilObject(refSpline))
        {
            ssv <- characteriseSplineStepVectors(object$refSpline, pointType=object$pointType)
            object$refLengths <- list(left=ssv$leftLength, right=ssv$rightLength)
        }
        
        return (object)
    },
    
    getAlphas = function (side = c("left","right"))
    {
        side <- match.arg(side)
        alphas <- numeric(0)
        for (i in 1:length(cosineDistributions[[side]]))
            alphas <- c(alphas, as.list(cosineDistributions[[side]][[i]])$alpha)
        return (alphas)
    },
    
    getCosineDistribution = function (pos, side = c("left","right"))
    {
        side <- match.arg(side)
        if ((pos < 1) || (pos > length(cosineDistributions[[side]])))
            return (NA)
        else
            return (cosineDistributions[[side]][[pos]])
    },
    
    getMaximumLength = function () { return (max(lengthDistributions$left$values)) },
    
    getPointType = function () { return (pointType) },
    
    getLengthDistribution = function (side = c("left","right"))
    {
        side <- match.arg(side)
        return (lengthDistributions[[side]])
    },
    
    getRefLeftLength = function () { return (refLengths$left) },
    
    getRefRightLength = function () { return (refLengths$right) },
    
    getRefSpline = function () { return (refSpline) },
    
    isAsymmetric = function ()
    {
        minLength <- min(length(cosineDistributions$left), length(cosineDistributions$right))
        return (!equivalent(cosineDistributions$left[1:minLength], cosineDistributions$right[1:minLength]))
    },
    
    summarise = function ()
    {
        if (.self$isAsymmetric())
        {
            labels <- c("Asymmetric model", "Alphas (left)", "Alphas (right)")
            values <- c(TRUE, implode(round(.self$getAlphas("left"),2),sep=", "), implode(round(.self$getAlphas("right"),2),sep=", "))
        }
        else
        {
            longerSide <- ifelse(length(cosineDistributions$left) > length(cosineDistributions$right), "left", "right")
            labels <- c("Asymmetric model", "Alphas")
            values <- c(FALSE, implode(round(.self$getAlphas(longerSide),2),sep=", "))
        }
        
        labels <- c(labels, "Ref tract lengths", "Length cutoff", "Point type")
        values <- c(values, paste(.self$getRefLeftLength(), " (left), ", .self$getRefRightLength(), " (right)", sep=""), .self$getMaximumLength(), .self$getPointType())
        return (list(labels=labels, values=values))
    }
))

calculateRescaledCosinesFromAngles <- function (angles, naRemove = TRUE)
{
    if (naRemove)
        angles <- angles[!is.na(angles)]
    cosines <- 0.5 * (cos(angles)+1)
    return (cosines)
}

createDataTableForSplines <- function (splines, refSpline, pointType, sessionPath = NULL, neighbourhood = NULL)
{
    if (!is.list(splines))
        report(OL$Error, "Spline tracts must be specified as a list of BSplineTract objects")
    if (!is(refSpline,"BSplineTract"))
        report(OL$Error, "The specified reference tract is not a BSplineTract object")
    
    nSplines <- length(splines)
    rssv <- characteriseSplineStepVectors(refSpline, pointType=pointType)
    
    leftLengths <- rep(NA, nSplines)
    rightLengths <- rep(NA, nSplines)
    similarityCosines <- matrix(NA, nrow=nSplines, ncol=(rssv$leftLength+rssv$rightLength))
    colnames(similarityCosines) <- c(paste("leftSimCosine",1:rssv$leftLength,sep=""), paste("rightSimCosine",1:rssv$rightLength,sep=""))
    
    for (i in seq_along(splines))
    {
        if (identical(splines[[i]], NA))
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
    if (!is.null(sessionPath))
        data <- cbind(data, data.frame(sessionPath=rep(sessionPath,nSplines)))
    if (!is.null(neighbourhood))
        data <- cbind(data, data.frame(x=neighbourhood$vectors[1,], y=neighbourhood$vectors[2,], z=neighbourhood$vectors[3,]))
    data <- cbind(data, similarityCosines)
    
    invisible (data)
}

readPntDataTable <- function (datasetName)
{
    if (file.exists(ensureFileSuffix(datasetName, "txt")))
        data <- read.table(ensureFileSuffix(datasetName,"txt"), stringsAsFactors=FALSE)
    else
    {
        # Dataset will have been created piecemeal by plough, so we need to collect the pieces
        fileNames <- list.files(dirname(datasetName))
        datasetStem <- ensureFileSuffix(datasetStem, NULL, strip="txt")
        match <- ore.search(ore("^",ore.escape(datasetStem),"\\.(\\d+)\\.txt$"), fileNames, simplify=FALSE)
        indices <- as.integer(groups(match, simplify=FALSE))
        fileNames <- fileNames[!is.na(indices)]
        indices <- indices[!is.na(indices)]
        data <- Reduce(rbind, lapply(fileNames[order(indices)], read.table, stringsAsFactors=FALSE))
        write.table(data, ensureFileSuffix(datasetName,"txt"))
        # unlink(fileNames)
    }
    
    return (data)
}

newUninformativeTractModelFromDataTable <- function (data, maxLength = NULL, weights = NULL)
{
    if (!is.null(weights))
    {
        if (length(weights) != nrow(data))
            report(OL$Error, "The weight vector must have the same length as the spline data table")
        
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

    model <- UninformativeTractModel$new(lengthDistributions=lengthDistributions, refLengths=refLengths, pointType=as.character(data$pointType[1]))
    invisible (model)
}

newMatchingTractModelFromDataTable <- function (data, refSpline, maxLength = NULL, lambda = NULL, alphaOffset = 0, weights = NULL, asymmetric = FALSE)
{
    if (is.null(weights))
    {
        if (is.null(data$sessionPath))
            report(OL$Error, "The \"sessionPath\" field must be present in the data table if weights are not specified")
        
        # Each subject needs normalising individually for the number of valid
        # candidate tracts available
        subjects <- factor(data$sessionPath)
        nTotalSplines <- tapply(data$leftLength, subjects, length)
        nValidSplines <- tapply(!is.na(data$leftLength), subjects, sum)
        weights <- unlist(lapply(seq_along(nValidSplines), function (i) rep(1,nTotalSplines[i]) / nValidSplines[i]))
    }
    else if (length(weights) != nrow(data))
        report(OL$Error, "The weight vector must have the same length as the spline data table")
    weights[is.na(data$leftLength)] <- NA
    
    refLeftLength <- length(grep("^leftSimCosine", colnames(data)))
    refRightLength <- length(grep("^rightSimCosine", colnames(data)))
    
    if (is.null(maxLength))
        maxLength <- max(data$leftLength, data$rightLength, refLeftLength, refRightLength, na.rm=TRUE)
    
    leftLengthDistribution <- fitMultinomialDistribution(data$leftLength, const=1, values=0:maxLength, weights=weights)
    rightLengthDistribution <- fitMultinomialDistribution(data$rightLength, const=1, values=0:maxLength, weights=weights)
    lengthDistributions <- list(left=leftLengthDistribution, right=rightLengthDistribution)
    
    cosineDistributions <- list(left=list(), right=list())
    for (side in c("left","right"))
    {
        for (i in 1:ifelse(side=="left",refLeftLength,refRightLength))
        {
            if (asymmetric)
                cosines <- data[[paste(side,"SimCosine",i,sep="")]]
            else
                cosines <- c(data[[paste("leftSimCosine",i,sep="")]], data[[paste("rightSimCosine",i,sep="")]])
            cosineWeights <- rep(weights, length(cosines)/length(weights))
            cosineWeights[is.na(cosines)] <- NA

            if (is.null(cosines) || sum(!is.na(cosines)) == 0)
                cosineDistributions[[side]] <- c(cosineDistributions[[side]], NA)
            else
                cosineDistributions[[side]] <- c(cosineDistributions[[side]], list(fitRegularisedBetaDistribution(cosines, lambda=lambda, alphaOffset=alphaOffset, weights=cosineWeights)))
        }
    }
    
    model <- MatchingTractModel$new(cosineDistributions=cosineDistributions, lengthDistributions=lengthDistributions, refSpline=refSpline, pointType=as.character(data$pointType[1]))
    invisible (model)
}

calculateUninformativeLogLikelihoodsForDataTable <- function (data, uninformativeModel)
{
    if (!is(uninformativeModel, "UninformativeTractModel"))
        report(OL$Error, "The specified model is not an UninformativeTractModel object")
    
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
    if (!is(matchingModel, "MatchingTractModel"))
        report(OL$Error, "The specified model is not a MatchingTractModel object")
    
    lls <- numeric(nrow(data))
    for (i in 1:nrow(data))
        lls[i] <- evaluateMultinomialDistribution(data$leftLength[i], matchingModel$getLengthDistribution("left"), log=TRUE) + evaluateMultinomialDistribution(data$rightLength[i], matchingModel$getLengthDistribution("right"), log=TRUE)
    
    for (j in 2:matchingModel$getRefLeftLength())
    {
        # Implicitly use a uniform distribution (i.e. fixed log likelihood
        # contribution of 0) if the model is not trained at this point
        if (is.list(matchingModel$getCosineDistribution(j)))
        {
            contribs <- evaluateBetaDistribution(data[[paste("leftSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j,"left"), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    for (j in 2:matchingModel$getRefRightLength())
    {
        if (is.list(matchingModel$getCosineDistribution(j)))
        {
            contribs <- evaluateBetaDistribution(data[[paste("rightSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j,"right"), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    
    return (lls)
}
