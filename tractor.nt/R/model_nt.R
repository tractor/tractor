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
    if (options$registerToReference && is.null(refSession))
        output(OL$Error, "The reference session must be specified if options$registerToReference is TRUE")
    
    streamSet <- newStreamlineSetTractFromProbtrack(session, seed, maxPathLength=options$maxPathLength)
    streamline <- newStreamlineTractFromSet(streamSet, method="median", lengthQuantile=options$lengthQuantile, originAtSeed=TRUE)
    if (options$registerToReference)
    {
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
