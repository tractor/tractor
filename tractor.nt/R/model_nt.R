createTractOptionList <- function (pointType = "knot", lengthQuantile = 0.99, registerToReference = TRUE, knotSpacing = NULL, maxPathLength = NULL)
{
    options <- list(pointType=pointType, lengthQuantile=lengthQuantile, registerToReference=registerToReference, knotSpacing=knotSpacing, maxPathLength=maxPathLength)
    class(options) <- "tractOptions"
    invisible (options)
}

transformStreamlineWithOptions <- function (options, streamline, session, refSession = NULL)
{
    if (options$registerToReference)
    {
        if (is.null(refSession))
            transform <- session$getTransformation("diffusion", "mni")
        else
            transform <- registerImages(session$getRegistrationTargetFileName("diffusion"), refSession$getRegistrationTargetFileName("diffusion"))
        
        streamline$transform(transform)
    }
    
    invisible (streamline)
}

streamlineTractWithOptions <- function (options, session, seed, refSession = NULL, nStreamlines = 5000, rightwardsVector = NULL)
{
    streamSource <- generateStreamlines(session$getTracker(), seed, nStreamlines, rightwardsVector)
    streamline <- streamSource$filter(medianOnly=TRUE, medianLengthQuantile=options$lengthQuantile)$getStreamlines(simplify=TRUE)
    
    invisible (transformStreamlineWithOptions(options, streamline, session, refSession))
}

splineTractWithOptions <- function (options, session, seed, refSession = NULL, nStreamlines = 5000, rightwardsVector = NULL)
{
    streamline <- streamlineTractWithOptions(options, session, seed, refSession, nStreamlines, rightwardsVector)
    spline <- newBSplineTractFromStreamline(streamline, knotSpacing=options$knotSpacing)
    
    invisible (spline)
}

referenceSplineTractWithOptions <- function (options, refSession, refSeed, nStreamlines = 5000, maxAngle = NULL)
{
    refOptions <- createTractOptionList(options$pointType, options$lengthQuantile, FALSE, NULL, options$maxPathLength)
    streamline <- streamlineTractWithOptions(refOptions, refSession, refSeed, nStreamlines=nStreamlines)
    refSpline <- newBSplineTractFromStreamlineWithConstraints(streamline, maxAngle=maxAngle)
    
    options$knotSpacing <- refSpline$getKnotSpacing()
    
    invisible (list(spline=refSpline, options=options))
}

calculateSplinesForNeighbourhood <- function (session, neighbourhood, reference, faThreshold = 0.2, nStreamlines = 5000)
{
    if (!is(reference,"ReferenceTract") || !is(reference$getTract(),"BSplineTract"))
        report(OL$Error, "The specified reference tract is not valid")
    
    referenceSteps <- calculateSplineStepVectors(reference$getTract(), reference$getTractOptions()$pointType)
    if (nrow(referenceSteps$right) >= 2)
        rightwardsVector <- referenceSteps$right[2,]
    else if (nrow(referenceSteps$left) >= 2)
        rightwardsVector <- (-referenceSteps$left[2,])
    else
        report(OL$Error, "The specified reference tract has no length on either side")
    
    fa <- session$getImageByType("fa", "diffusion")
    
    seeds <- neighbourhood$vectors
    nSeeds <- ncol(seeds)
    middle <- (nSeeds %/% 2) + 1
    
    splines <- vector("list", nSeeds)
    for (i in seq_len(nSeeds))
    {
        seed <- seeds[,i]
        report(OL$Verbose, "Current seed point is ", implode(seed,sep=","), " (#{i}/#{nSeeds})")
        
        if (any(seed <= 0 | seed > fa$getDimensions()))
        {
            report(OL$Verbose, "Skipping seed point because it's out of bounds")
            splines[[i]] <- NA
        }
        else if (!is.na(fa$getDataAtPoint(seed)) && (fa$getDataAtPoint(seed) < faThreshold) && (i != middle))
        {
            report(OL$Verbose, "Skipping seed point because FA < ", faThreshold)
            splines[[i]] <- NA
        }
        else
            splines[[i]] <- splineTractWithOptions(reference$getTractOptions(), session, seed, reference$getSourceSession(), nStreamlines, rightwardsVector)
    }
    
    invisible (splines)
}

calculatePosteriorsForDataTable <- function (data, matchingModel)
{
    if (is.null(data$sessionPath))
        data <- cbind(data, data.frame(sessionPath=rep("",nrow(data))))
    
    subjects <- factor(data$sessionPath)
    nSubjects <- nlevels(subjects)
    nSplines <- tapply(data$sessionPath, subjects, length)
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

runMatchingEMForDataTable <- function (data, refSpline, lengthCutoff = NULL, lambda = NULL, alphaOffset = 0, nullPrior = NULL, asymmetricModel = FALSE)
{
    if (!is(refSpline,"BSplineTract"))
        report(OL$Error, "Reference tract must be specified as a BSplineTract object")
    if (is.null(data$sessionPath))
        report(OL$Error, "The \"sessionPath\" field must be present in the data table")
    
    subjects <- factor(data$sessionPath)
    nSubjects <- nlevels(subjects)
    nSplines <- tapply(data$sessionPath, subjects, length)
    validSplines <- tapply(!is.na(data$leftLength), subjects, "[")
    nValidSplines <- tapply(!is.na(data$leftLength), subjects, sum)
    
    if (is.null(nullPrior))
        nullPriors <- 1/(nValidSplines+1)
    else
        nullPriors <- rep(nullPrior, nSubjects)
    report(OL$Verbose, "Null priors are ", implode(round(nullPriors,5),sep=", "))
    
    nullPriors <- as.list(nullPriors)
    
    if (is.null(lengthCutoff))
        lengthCutoff <- max(data$leftLength, data$rightLength, na.rm=TRUE)
    report(OL$Verbose, "Length cutoff is ", lengthCutoff)
    
    tractPriors <- list()
    for (s in levels(subjects))
    {
        priors <- rep(NA, nSplines[[s]])
        priors[validSplines[[s]]] <- (1-nullPriors[[s]])/nValidSplines[[s]]
        tractPriors[[s]] <- priors
    }
    
    previousLogEvidence <- -Inf
    previousAlphas <- NULL
    report(OL$Info, "Starting EM algorithm")
    
    repeat
    {
        matchingModel <- newMatchingTractModelFromDataTable(data, refSpline, lengthCutoff, lambda=lambda, alphaOffset=alphaOffset, weights=unlist(tractPriors), asymmetric=asymmetricModel)
        uninformativeModel <- newUninformativeTractModelFromDataTable(data, lengthCutoff, weights=(1-unlist(tractPriors)))
        
        matchedLogLikelihoods <- calculateMatchedLogLikelihoodsForDataTable(data, matchingModel)
        uninformativeLogLikelihoods <- calculateUninformativeLogLikelihoodsForDataTable(data, uninformativeModel)
        
        nMatchedBetter <- sum(matchedLogLikelihoods>uninformativeLogLikelihoods, na.rm=TRUE)
        report(OL$Verbose, nMatchedBetter, " tracts are explained better by the matching model")
        
        alphas <- matchingModel$getAlphas()
        if (!is.null(previousAlphas))
        {
            meanDifference <- mean(abs(alphas - previousAlphas))
            report(OL$Verbose, "Mean difference in alpha parameters is ", meanDifference)
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
        
        report(OL$Verbose, "Log-evidence is ", logEvidence)        
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
