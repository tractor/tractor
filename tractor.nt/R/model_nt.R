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

splineTractWithOptions <- function (options, session, seed, refSession = NULL, nSamples = 5000, rightwardsVector = NULL)
{
    if (!isTractOptionList(options))
        output(OL$Error, "Tract representation options must be specified as a TractOptionList")
    
    streamSet <- newStreamlineSetTractFromProbtrack(session, seed, nSamples=nSamples, maxPathLength=options$maxPathLength, rightwardsVector=rightwardsVector)
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

calculateSplinesForNeighbourhood <- function (testSession, testSeed, reference, searchWidth = 7, faThreshold = 0.2, nSamples = 5000)
{
    if (!isReferenceTract(reference) || !isBSplineTract(reference))
        output(OL$Error, "The specified reference tract is not valid")
    
    referenceSteps <- calculateSplineStepVectors(reference$getTract(), reference$getTractOptions()$pointType)
    if (nrow(referenceSteps$right) >= 2)
        rightwardsVector <- referenceSteps$right[2,]
    else if (nrow(referenceSteps$left) >= 2)
        rightwardsVector <- (-referenceSteps$left[2,])
    else
        output(OL$Error, "The specified reference tract has no length on either side")
    
    fa <- testSession$getImageByType("fa")
    
    nSeeds <- searchWidth ^ 3
    stepVectors <- buildStepVectors(searchWidth)
    middle <- (nSeeds %/% 2) + 1
    
    splines <- list()
    for (i in 1:nSeeds)
    {
        seed <- testSeed + stepVectors[,i]
        output(OL$Info, "Current seed point is ", implode(seed,sep=","), " (", i, "/", nSeeds, ")")
        
        if (!is.na(fa$getDataAtPoint(seed)) && (fa$getDataAtPoint(seed) < faThreshold) && (i != middle))
        {
            output(OL$Info, "Skipping seed point because FA < ", faThreshold)
            splines <- c(splines, list(NA))
        }
        else
        {
            spline <- splineTractWithOptions(reference$getTractOptions(), testSession, seed, reference$getSourceSession(), nSamples, rightwardsVector)
            splines <- c(splines, list(spline))
        }
    }
    
    invisible (splines)
}

calculatePosteriorsForDataTable <- function (data, matchingModel)
{
    if (is.null(data$subject))
        data <- cbind(data, data.frame(subject=rep(1,nrow(data))))
    
    subjects <- factor(data$subject)
    nSubjects <- nlevels(subjects)
    nSplines <- tapply(data$subject, subjects, length)
    validSplines <- tapply(!is.na(data$leftLength), subjects, "[")
    nValidSplines <- tapply(!is.na(data$leftLength), subjects, sum)
    
    matchedLogLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, matchingModel)
    
    tractPosteriors <- nullPosteriors <- list()
    for (s in levels(subjects))
    {
        priors <- rep(NA, nSplines[[s]])
        priors[validSplines[[s]]] <- 1 / nValidSplines[[s]]
        
        posteriors <- calculatePosteriorsFromLogLikelihoods(matchedLogLikelihoods[subjects==s], rep(1,nValidSplines[[s]]), priors)
        tractPosteriors[[s]] <- posteriors$tractPosteriors
        nullPosteriors[[s]] <- posteriors$nullPosterior
    }
    
    results <- list(tp=tractPosteriors, np=nullPosteriors, mm=matchingModel)
    invisible (results)
}

runMatchingEMForDataTable <- function (data, refSpline, lengthCutoff = NULL, lambda = NULL, alphaOffset = 0, nullPrior = NULL)
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
    
    nullPriors <- as.list(nullPriors)
    
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
        matchingModel <- newMatchingTractModelFromDataTable(data, refSpline, lengthCutoff, lambda=lambda, alphaOffset=alphaOffset, weights=unlist(tractPriors))
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
    
    if (nullPosterior == Inf)
    {
        # Bail out an overflow
        logEvidence <- log(nullPrior) + sum(nonmatchedLogLikelihoods, na.rm=TRUE)
        tractPosteriors[!is.na(tractPosteriors)] <- 0
        posteriors <- list(tractPosteriors=tractPosteriors, nullPosterior=1, logEvidence=logEvidence)
    }
    else
    {
        evidence <- nullPosterior + sum(tractPosteriors, na.rm=TRUE)
        tractPosteriors <- tractPosteriors / evidence
        nullPosterior <- nullPosterior / evidence
        posteriors <- list(tractPosteriors=tractPosteriors, nullPosterior=nullPosterior, logEvidence=log(evidence)+maxTractPosterior)
    }
    
    return (posteriors)
}
