createTractOptionList <- function (pointType = "knot", lengthQuantile = 0.99, registerToReference = TRUE, knotSpacing = NULL, maxPathLength = NULL)
{
    options <- list(pointType=pointType, lengthQuantile=lengthQuantile, registerToReference=registerToReference, knotSpacing=knotSpacing, maxPathLength=maxPathLength)
    class(options) <- c("options.tract", "list")
    invisible (options)
}

isTractOptionList <- function (object)
{
    return ("options.tract" %in% class(object))
}

splineTractWithOptions <- function (options, session, seed, refSession = NULL)
{
    if (!isTractOptionList(options))
        output(OL$Error, "Tract representation options must be specified as a TractOptionList")
    
    streamSet <- newStreamlineSetTractFromProbtrack(session, seed, maxPathLength=options$maxPathLength)
    streamline <- newStreamlineTractFromSet(streamSet, method="median", lengthQuantile=options$lengthQuantile, originAtSeed=TRUE)
    if (options$registerToReference)
    {
        if (is.null(refSession))
            transform <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        else
            transform <- newAffineTransform3DFromFlirt(session$getImageFileNameByType("t2"), refSession$getImageFileNameByType("t2"))
        
        streamline <- newStreamlineTractByTransformation(streamline, transform)
    }
    spline <- newBSplineTractFromStreamline(streamline, knotSpacing=options$knotSpacing)
    
    invisible (spline)
}

referenceSplineTractWithOptions <- function (options, refSession, refSeed)
{
    refOptions <- createTractOptionList(options$pointType, options$lengthQuantile, FALSE, NULL, options$maxPathLength)
    refSpline <- splineTractWithOptions(refOptions, refSession, refSeed)
    
    options$knotSpacing <- refSpline$getKnotSpacing()
    
    invisible (list(spline=refSpline, options=options))
}

buildMaxLikelihoodMatchingModel <- function (refSession, refSeed, otherSessions, otherSeeds, options, lengthCutoff = 50)
{
    if (length(otherSessions) != nrow(otherSeeds))
        output(OL$Error, "Dimensions of matching session list and seed matrix do not match")
    
    reference <- referenceSplineTractWithOptions(options, refSession, refSeed)
    
    otherSplines <- list()
    for (i in seq_along(otherSessions))
    {
        spline <- splineTractWithOptions(reference$options, otherSessions[[i]], otherSeeds[i,], refSession)
        otherSplines <- c(otherSplines, list(spline))
    }
    
    matchingModel <- newMatchingTractModelFromSplines(reference$spline, otherSplines, lengthCutoff, reference$options$pointType)
    invisible (list(model=matchingModel, splines=otherSplines, options=reference$options))
}

calculateSplinesForNeighbourhood <- function (testSession, testSeed, refSession, options, searchWidth = 7, avfThreshold = 0.2)
{
    avf <- testSession$getImageByType("avf")
    
    nSessions <- searchWidth ^ 3
    stepVectors <- buildStepVectors(searchWidth)
    middle <- (nSessions %/% 2) + 1
    
    splines <- list()
    for (i in 1:nSessions)
    {
        seed <- testSeed + stepVectors[,i]
        output(OL$Info, "Current seed point is ", implode(seed,sep=","), " (", i, "/", nSessions, ")")
        
        if (!is.na(avf$getDataAtPoint(seed)) && (avf$getDataAtPoint(seed) < avfThreshold) && (i != middle))
        {
            output(OL$Info, "Skipping seed point because AVF < ", avfThreshold)
            splines <- c(splines, list(NA))
        }
        else
        {
            spline <- splineTractWithOptions(options, testSession, seed, refSession)
            splines <- c(splines, list(spline))
        }
    }
    
    invisible (splines)
}

calculatePosteriorsForSplines <- function (splines, matchingModel, nonmatchingModel, nullPrior = NULL)
{
    if (!is.list(splines))
        output(OL$Error, "Tracts must be specified as a list of BSplineTract objects")
    
    validSessions <- which(!is.na(splines))
    nValidSessions <- length(validSessions)
    output(OL$Info, "Using ", nValidSessions, " of ", length(splines), " possible seed points")
    
    if (is.null(nullPrior))
        nullPrior <- 1/(nValidSessions+1)
    tractPriors <- rep((1-nullPrior)/nValidSessions, nValidSessions)
    output(OL$Info, "Null match probability is ", round(nullPrior,5), "; other prior values are ", round(tractPriors[1],5))
    
    matchedLogLikelihoods <- rep(NA, nValidSessions)
    nonmatchedLogLikelihoods <- rep(NA, nValidSessions)
    for (i in validSessions)
    {
        j <- which(validSessions == i)
        matchedLogLikelihoods[j] <- calculateMatchedLogLikelihoodForSpline(splines[[i]], matchingModel)
        nonmatchedLogLikelihoods[j] <- calculateNonmatchedLogLikelihoodForSpline(splines[[i]], nonmatchingModel)
    }
    
    oneModelPosteriors <- calculatePosteriorsFromLikelihoods(matchedLogLikelihoods, rep(1,nValidSessions), tractPriors, 0)
    twoModelPosteriors <- calculatePosteriorsFromLikelihoods(matchedLogLikelihoods, nonmatchedLogLikelihoods, tractPriors, nullPrior)
    
    results <- list(tp1=oneModelPosteriors$tractPosteriors,
                    np1=oneModelPosteriors$nullPosterior,
                    tp2=twoModelPosteriors$tractPosteriors,
                    np2=twoModelPosteriors$nullPosterior,
                    mll=matchedLogLikelihoods,
                    nll=nonmatchedLogLikelihoods,
                    validSessions=validSessions)    
    invisible (results)
}

runMatchingEMForDataTable <- function (data, refSpline, lengthCutoff = NULL, lambda = NULL, nullPrior = NULL)
{
    if (!isBSplineTract(refSpline))
        output(OL$Error, "Reference tract must be specified as a BSplineTract object")
    if (is.null(data$subject))
        output(OL$Error, "The 'subject' field must be present in the data table")
    
    subjects <- factor(data$subject)
    nSubjects <- nlevels(subjects)
    nSplines <- tapply(data$subject, subjects, length)
    validSplines <- tapply(!is.na(data$leftLength), subjects, "[")
    nValidSplines <- tapply(!is.na(data$leftLength), subjects, sum)
    
    if (is.null(nullPrior))
        nullPriors <- 1/(nValidSplines+1)
    else
        nullPriors <- rep(nullPrior, nSubjects)
    output(OL$Verbose, "Null priors are ", implode(round(nullPriors,5),sep=", "))
    
    if (is.null(lengthCutoff))
        lengthCutoff <- max(data$leftLength, data$rightLength, na.rm=TRUE)
    output(OL$Verbose, "Length cutoff is ", lengthCutoff)
    
    tractPriors <- list()
    for (s in levels(subjects))
    {
        priors <- rep(NA, nSplines[[s]])
        priors[validSplines[[s]]] <- (1-nullPriors[[s]])/nValidSplines[[s]]
        tractPriors[[s]] <- priors
    }
    
    previousLogEvidence <- -Inf
    previousAlphas <- NULL
    output(OL$Info, "Starting EM algorithm")
    
    repeat
    {
        matchingModel <- newMatchingTractModelFromDataTable(data, refSpline, lengthCutoff, lambda=lambda, weights=unlist(tractPriors))
        uninformativeModel <- newUninformativeTractModelFromDataTable(data, lengthCutoff, weights=(1-unlist(tractPriors)))
        
        matchedLogLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, matchingModel)
        uninformativeLogLikelihoods <- calculateUninformativeLogLikelihoodsForDataTable(data, uninformativeModel)
        
        nMatchedBetter <- sum(matchedLogLikelihoods>uninformativeLogLikelihoods, na.rm=TRUE)
        output(OL$Verbose, nMatchedBetter, " tracts are explained better by the matching model")
        
        alphas <- matchingModel$getAlphas()
        if (!is.null(previousAlphas))
        {
            meanDifference <- mean(abs(alphas - previousAlphas))
            output(OL$Verbose, "Mean difference in alpha parameters is ", meanDifference)
            if (meanDifference < 0.1)
                break
        }
        previousAlphas <- alphas
        
        logEvidence <- 0
        for (s in levels(subjects))
        {
            posteriors <- calculatePosteriorsFromLogLikelihoods(matchedLogLikelihoods[subjects==s], uninformativeLogLikelihoods[subjects==s], tractPriors[[s]], nullPriors[[s]])
            tractPriors[[s]] <- posteriors$tractPosteriors
            nullPriors[[s]] <- posteriors$nullPosterior
            logEvidence <- logEvidence + posteriors$logEvidence
        }
        
        output(OL$Verbose, "Log-evidence is ", logEvidence)        
        if (abs(logEvidence-previousLogEvidence) < 0.1)
            break
        previousLogEvidence <- logEvidence
    }
    
    results <- list(tp=tractPriors, np=nullPriors, mm=matchingModel, um=uninformativeModel)
    invisible (results)
}

calculatePosteriorsFromLogLikelihoods <- function (matchedLogLikelihoods, nonmatchedLogLikelihoods, tractPriors, nullPrior = 0)
{
    nTracts <- length(tractPriors)
    
    tractPosteriors <- matchedLogLikelihoods + log(tractPriors)
    for (j in 1:nTracts)
        tractPosteriors[j] <- tractPosteriors[j] + sum(nonmatchedLogLikelihoods[-j], na.rm=TRUE)
    maxTractPosterior <- max(tractPosteriors, na.rm=TRUE)
    tractPosteriors <- exp(tractPosteriors - maxTractPosterior)
    nullPosterior <- log(nullPrior) + sum(nonmatchedLogLikelihoods, na.rm=TRUE)
    nullPosterior <- exp(nullPosterior - maxTractPosterior)
    evidence <- nullPosterior + sum(tractPosteriors, na.rm=TRUE)
    tractPosteriors <- tractPosteriors / evidence
    nullPosterior <- nullPosterior / evidence
    
    posteriors <- list(tractPosteriors=tractPosteriors, nullPosterior=nullPosterior, logEvidence=log(evidence)+maxTractPosterior)
    return (posteriors)
}
