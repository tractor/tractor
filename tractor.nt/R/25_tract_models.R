#' The UninformativeTractModel class
#'
#' This class represents the "null", or uninformative, model used in
#' probabilistic neighbourhood tractography (PNT): a model of candidate tract
#' lengths which does not take their similarity to a reference tract's shape
#' into account. It is used as an alternative to a [MatchingTractModel]
#' during model fitting and evaluation, so that candidate tracts can be
#' judged as either matching the reference shape or not. Objects of this
#' class are usually created by [newUninformativeTractModelFromDataTable()].
#'
#' @field lengthDistributions A list with elements `left` and `right`, each a
#'   `multinomialDistribution` object (see [fitMultinomialDistribution()])
#'   giving the distribution of candidate tract lengths, in numbers of points,
#'   on the corresponding side of the seed.
#' @field refLengths A list with elements `left` and `right`, giving the
#'   number of points on each side of the seed in the associated reference
#'   tract.
#' @field pointType A string, either `"control"` or `"knot"`, indicating the
#'   type of point used to represent tract shape and length.
#'
#' @export
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
        "Summarise the uninformative tract model"
        labels <- c("Ref tract lengths", "Length cutoff", "Point type")
        values <- c(paste(getRefLeftLength(), " (left), ", getRefRightLength(), " (right)",sep=""), getMaximumLength(), getPointType())
        return (list(labels=labels, values=values))
    }
))

#' The MatchingTractModel class
#'
#' This class represents a tract-matching model for probabilistic
#' neighbourhood tractography (PNT): a model of both the length and, more
#' importantly, the shape of candidate tracts relative to a reference tract.
#' Shape is captured by beta distributions fitted to the (rescaled) cosines
#' of the angles between corresponding step vectors of each candidate and the
#' reference, at each point along the tract. Objects of this class are
#' usually created by [newMatchingTractModelFromDataTable()] or, iteratively,
#' by [runMatchingEMForDataTable()].
#'
#' @field cosineDistributions A list with elements `left` and `right`, each a
#'   list of `betaDistribution` objects (see [fitBetaDistribution()]), one
#'   per point moving outwards from the seed, describing the distribution of
#'   rescaled cosine similarity to the reference tract's shape at that point.
#' @field lengthDistributions A list with elements `left` and `right`, each a
#'   `multinomialDistribution` object (see [fitMultinomialDistribution()])
#'   giving the distribution of candidate tract lengths, in numbers of
#'   points, on the corresponding side of the seed.
#' @field refSpline A [BSplineTract] object giving the reference tract that
#'   candidates are compared against.
#' @field refLengths A list with elements `left` and `right`, giving the
#'   number of points on each side of the seed in `refSpline`.
#' @field pointType A string, either `"control"` or `"knot"`, indicating the
#'   type of point used to represent tract shape and length.
#'
#' @export
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
        "Collect the alpha parameters of the cosine distributions on the specified side"
        side <- match.arg(side)
        alphas <- numeric(0)
        for (i in 1:length(cosineDistributions[[side]]))
            alphas <- c(alphas, as.list(cosineDistributions[[side]][[i]])$alpha)
        return (alphas)
    },
    
    getCosineDistribution = function (pos, side = c("left","right"))
    {
        "Retrieve the fitted cosine distribution at the specified point and side, or NA if out of range"
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
        "Return TRUE if the cosine distributions differ between the left and right sides"
        minLength <- min(length(cosineDistributions$left), length(cosineDistributions$right))
        return (!equivalent(cosineDistributions$left[1:minLength], cosineDistributions$right[1:minLength]))
    },
    
    summarise = function ()
    {
        "Summarise the matching tract model"
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

#' Rescale angles to a beta-distributable range
#'
#' This function converts a vector of angles, in radians, to rescaled
#' cosines lying in the range \eqn{[0,1]}, suitable for modelling with a beta
#' distribution (see [fitBetaDistribution()]). An angle of zero maps to a
#' value of 1, and an angle of \eqn{\pi} maps to a value of 0.
#'
#' @param angles A numeric vector of angles, in radians.
#' @param naRemove Boolean value: if `TRUE`, the default, missing values are
#'   dropped from `angles` before rescaling.
#' @return A numeric vector of rescaled cosines.
#' @author Jon Clayden
#' @export
calculateRescaledCosinesFromAngles <- function (angles, naRemove = TRUE)
{
    if (naRemove)
        angles <- angles[!is.na(angles)]
    cosines <- 0.5 * (cos(angles)+1)
    return (cosines)
}

#' Create a PNT data table from candidate B-spline tracts
#'
#' This function builds a data frame summarising a set of candidate
#' [BSplineTract] objects relative to a reference tract, in the form used
#' throughout the rest of the probabilistic neighbourhood tractography (PNT)
#' pipeline (see [readPntDataTable()], [newMatchingTractModelFromDataTable()]
#' and related functions). Each row corresponds to one candidate; elements of
#' `splines` which are `NA` produce a row of missing values.
#'
#' @param splines A list of [BSplineTract] objects (or `NA`), one per
#'   candidate seed point.
#' @param refSpline A [BSplineTract] object giving the reference tract that
#'   candidates are compared against.
#' @param pointType A string, either `"control"` or `"knot"`, indicating the
#'   type of point used to represent tract shape and length.
#' @param sessionPath An optional string giving the path to the session that
#'   the candidates were generated from, stored in a `sessionPath` column if
#'   given.
#' @param neighbourhood An optional neighbourhood information object, as
#'   created by [tractor.base::createNeighbourhoodInfo()], whose seed point
#'   offset vectors are stored in `x`, `y` and `z` columns if given.
#' @return A data frame with one row per element of `splines`, giving its
#'   point type, its length (in points) on the left and right of the seed,
#'   and the rescaled cosine similarity to the reference tract's shape at
#'   each point on the left and right, in columns named `leftSimCosineN` and
#'   `rightSimCosineN`.
#' @author Jon Clayden
#' @export
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

#' Read a PNT data table
#'
#' This function reads a probabilistic neighbourhood tractography (PNT)
#' dataset, as created by [createDataTableForSplines()], from file. If the
#' complete dataset file does not exist, but pieces of it created separately
#' (for example by `plough`) do, these are combined, the combined file is
#' written out, and the pieces are removed.
#'
#' @param datasetName A string giving the name of the dataset, with or
#'   without a ".txt" extension.
#' @return A data frame containing the dataset.
#' @author Jon Clayden
#' @export
readPntDataTable <- function (datasetName)
{
    if (file.exists(ensureFileSuffix(datasetName, "txt")))
        data <- read.table(ensureFileSuffix(datasetName,"txt"), colClasses=c(sessionPath="character"), stringsAsFactors=FALSE)
    else
    {
        # Dataset will have been created piecemeal by plough, so we need to collect the pieces
        fileNames <- list.files(dirname(datasetName))
        datasetStem <- ensureFileSuffix(datasetName, NULL, strip="txt")
        match <- ore.search(ore("^",ore.escape(datasetStem),"\\.(\\d+)\\.txt$"), fileNames, simplify=FALSE)
        indices <- as.integer(groups(match, simplify=FALSE))
        fileNames <- fileNames[!is.na(indices)]
        indices <- indices[!is.na(indices)]
        data <- Reduce(rbind, lapply(fileNames[order(indices)], read.table, colClasses=c(sessionPath="character"), stringsAsFactors=FALSE))
        write.table(data, ensureFileSuffix(datasetName,"txt"))
        unlink(fileNames)
    }
    
    return (data)
}

#' Fit an uninformative tract model to a PNT data table
#'
#' This function fits an [UninformativeTractModel] to a PNT data table (see
#' [createDataTableForSplines()] and [readPntDataTable()]), by fitting
#' multinomial distributions to the observed candidate tract lengths on the
#' left and right of the seed.
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()].
#' @param maxLength An integer giving the maximum tract length, in points, to
#'   allow for. The default, `NULL`, uses the maximum length observed in
#'   `data`.
#' @param weights An optional numeric vector of weights, of the same length
#'   as the number of rows in `data`, giving the contribution of each
#'   candidate to the fit. The default, `NULL`, weights all candidates
#'   equally.
#' @return An [UninformativeTractModel] object.
#' @author Jon Clayden
#' @export
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

#' Fit a tract-matching model to a PNT data table
#'
#' This function fits a [MatchingTractModel] to a PNT data table (see
#' [createDataTableForSplines()] and [readPntDataTable()]), by fitting
#' multinomial distributions to the observed candidate tract lengths, and
#' (optionally regularised) beta distributions to the rescaled cosine
#' similarities between each candidate and the reference tract's shape, at
#' each point on the left and right of the seed.
#'
#' If `weights` is not specified explicitly, each subject (as identified by
#' the `sessionPath` column of `data`) is given equal total weight, shared
#' equally between its valid candidates. This is appropriate when each
#' candidate's prior probability of being the correct match is uniform, as in
#' the first iteration of [runMatchingEMForDataTable()].
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()].
#' @param refSpline A [BSplineTract] object giving the reference tract.
#' @param maxLength An integer giving the maximum tract length, in points, to
#'   allow for. The default, `NULL`, uses the maximum length observed in
#'   `data` or implied by `refSpline`.
#' @param lambda A regularisation parameter passed to
#'   [fitRegularisedBetaDistribution()], shrinking the fitted alpha
#'   parameters towards 1 (an uninformative cosine distribution). The
#'   default, `NULL`, disables regularisation.
#' @param alphaOffset A number added to each fitted alpha parameter after
#'   regularisation; see [fitRegularisedBetaDistribution()].
#' @param weights An optional numeric vector of weights, of the same length
#'   as the number of rows in `data`, giving the contribution of each
#'   candidate to the fit. The default, `NULL`, calculates weights
#'   automatically, as described above.
#' @param asymmetric Boolean value: if `TRUE`, separate cosine distributions
#'   are fitted for the left and right sides of the seed; otherwise data from
#'   both sides are pooled at each point.
#' @return A [MatchingTractModel] object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#'
#' For the probabilistic neighbourhood tractography method specifically, see
#'
#' J.D. Clayden, A.J. Storkey & M.E. Bastin (2007). A probabilistic
#' model-based approach to consistent white matter tract segmentation. IEEE
#' Transactions on Medical Imaging 26(11):1555-1561.
#' @export
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

#' Evaluate an uninformative tract model against a PNT data table
#'
#' This function calculates the log-likelihood of each candidate tract in a
#' PNT data table under the specified [UninformativeTractModel], based on its
#' length alone (with a fixed contribution assuming a uniform, 50/50 shape
#' match at each point up to the reference tract's length).
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()].
#' @param uninformativeModel An [UninformativeTractModel] object.
#' @return A numeric vector of log-likelihoods, one per row of `data`.
#' @author Jon Clayden
#' @export
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

#' Evaluate a tract-matching model against a PNT data table
#'
#' This function calculates the log-likelihood of each candidate tract in a
#' PNT data table under the specified [MatchingTractModel], combining
#' contributions from the length distributions and, at each point along the
#' tract for which the model has been trained, the fitted cosine similarity
#' distributions. Points beyond those the model was trained on implicitly
#' contribute a log-likelihood of zero (i.e. a uniform distribution).
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()].
#' @param matchingModel A [MatchingTractModel] object.
#' @return A numeric vector of log-likelihoods, one per row of `data`.
#' @author Jon Clayden
#' @export
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
        if (is.list(matchingModel$getCosineDistribution(j,"left")))
        {
            contribs <- evaluateBetaDistribution(data[[paste("leftSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j,"left"), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    for (j in 2:matchingModel$getRefRightLength())
    {
        if (is.list(matchingModel$getCosineDistribution(j,"right")))
        {
            contribs <- evaluateBetaDistribution(data[[paste("rightSimCosine",j,sep="")]], matchingModel$getCosineDistribution(j,"right"), log=TRUE)
            lls <- lls + replace(contribs, is.na(contribs), 0)
        }
    }
    
    return (lls)
}
