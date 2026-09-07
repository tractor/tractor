#' Create a set of tract generation options
#'
#' This function creates a `tractOptions` object recording the options used
#' to generate reference and candidate tracts for neighbourhood tractography,
#' for later reuse. It is stored with a [ReferenceTract] object, and used by
#' [streamlineTractWithOptions()] and related functions.
#'
#' @param pointType A string, either `"control"` or `"knot"` (the default),
#'   indicating the type of point used to represent tract shape and length.
#' @param lengthQuantile A number giving the quantile of streamline length to
#'   use when filtering generated streamlines down to a single, median-like
#'   representative (see [tractor.track::generateStreamlines()]).
#' @param registerToReference Boolean value: if `TRUE`, the default,
#'   generated streamlines are transformed into the reference session's (or
#'   standard) space before further processing.
#' @param knotSpacing A number giving the fixed spacing between B-spline
#'   knots, in mm, or `NULL` to choose a spacing automatically (see
#'   [newBSplineTractFromStreamline()]).
#' @param maxPathLength An integer giving the maximum number of knots to
#'   allow along a candidate tract, or `NULL` for no limit.
#' @return A `tractOptions` object, a classed list with the given elements.
#' @author Jon Clayden
#' @export
createTractOptionList <- function (pointType = "knot", lengthQuantile = 0.99, registerToReference = TRUE, knotSpacing = NULL, maxPathLength = NULL)
{
    options <- list(pointType=pointType, lengthQuantile=lengthQuantile, registerToReference=registerToReference, knotSpacing=knotSpacing, maxPathLength=maxPathLength)
    class(options) <- "tractOptions"
    invisible (options)
}

#' Transform a streamline into reference space
#'
#' This function transforms a streamline into the space of a reference
#' session, or into standard (MNI) space, if requested by the given tract
#' generation options.
#'
#' @param options A `tractOptions` object, as created by
#'   [createTractOptionList()].
#' @param streamline A [tractor.track::Streamline] object to transform.
#' @param session The [tractor.session::MriSession] that `streamline` was
#'   generated from.
#' @param refSession An optional [tractor.session::MriSession] object giving
#'   the session to register `streamline` to. The default, `NULL`, registers
#'   to standard (MNI) space instead.
#' @return The transformed streamline, returned invisibly. If
#'   `options$registerToReference` is `FALSE`, `streamline` is returned
#'   unchanged.
#' @author Jon Clayden
#' @export
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

#' Generate a representative streamline from a seed point
#'
#' This function generates a set of streamlines from a seed point, filters
#' them down to a single, representative streamline close to the median
#' length, and transforms it into reference space according to the given
#' tract generation options.
#'
#' @param options A `tractOptions` object, as created by
#'   [createTractOptionList()].
#' @param session The [tractor.session::MriSession] to generate streamlines
#'   within.
#' @param seed A numeric vector giving the seed point, in the units expected
#'   by [tractor.track::generateStreamlines()].
#' @param refSession An optional [tractor.session::MriSession] object to
#'   register the representative streamline to. The default, `NULL`,
#'   registers to standard (MNI) space instead.
#' @param nStreamlines The number of streamlines to generate from the seed
#'   point.
#' @param rightwardsVector An optional numeric vector used to disambiguate
#'   the direction of asymmetric tracking; see
#'   [tractor.track::generateStreamlines()].
#' @return The representative [tractor.track::Streamline] object, in
#'   reference space, returned invisibly.
#' @author Jon Clayden
#' @export
streamlineTractWithOptions <- function (options, session, seed, refSession = NULL, nStreamlines = 5000, rightwardsVector = NULL)
{
    streamSource <- generateStreamlines(session$getTracker(), seed, nStreamlines, rightwardsVector)
    streamline <- streamSource$filter(medianOnly=TRUE, medianLengthQuantile=options$lengthQuantile)$getStreamlines(simplify=TRUE)
    
    invisible (transformStreamlineWithOptions(options, streamline, session, refSession))
}

#' Generate a B-spline tract from a seed point
#'
#' This function generates a representative streamline from a seed point,
#' using [streamlineTractWithOptions()], and fits a [BSplineTract] to it
#' using [newBSplineTractFromStreamline()].
#'
#' @param options A `tractOptions` object, as created by
#'   [createTractOptionList()].
#' @param session The [tractor.session::MriSession] to generate streamlines
#'   within.
#' @param seed A numeric vector giving the seed point, in the units expected
#'   by [tractor.track::generateStreamlines()].
#' @param refSession An optional [tractor.session::MriSession] object to
#'   register the streamline to before fitting. The default, `NULL`,
#'   registers to standard (MNI) space instead.
#' @param nStreamlines The number of streamlines to generate from the seed
#'   point.
#' @param rightwardsVector An optional numeric vector used to disambiguate
#'   the direction of asymmetric tracking; see
#'   [tractor.track::generateStreamlines()].
#' @return A [BSplineTract] object, or `NA` if no adequate fit could be
#'   obtained. The result is returned invisibly.
#' @author Jon Clayden
#' @export
splineTractWithOptions <- function (options, session, seed, refSession = NULL, nStreamlines = 5000, rightwardsVector = NULL)
{
    streamline <- streamlineTractWithOptions(options, session, seed, refSession, nStreamlines, rightwardsVector)
    spline <- newBSplineTractFromStreamline(streamline, knotSpacing=options$knotSpacing)
    
    invisible (spline)
}

#' Generate a reference B-spline tract from a seed point
#'
#' This function generates a representative streamline from a seed point in
#' the reference session (without registering it elsewhere), and fits a
#' [BSplineTract] to it using
#' [newBSplineTractFromStreamlineWithConstraints()], trimming away any
#' aberrant sections distal to the seed. The knot spacing used is recorded in
#' the returned options, for reuse when generating candidate tracts to
#' compare against this reference.
#'
#' @param options A `tractOptions` object, as created by
#'   [createTractOptionList()]. Its `registerToReference` element is ignored
#'   and treated as `FALSE`.
#' @param refSession The [tractor.session::MriSession] to generate the
#'   reference streamline within.
#' @param refSeed A numeric vector giving the seed point, in the units
#'   expected by [tractor.track::generateStreamlines()].
#' @param nStreamlines The number of streamlines to generate from the seed
#'   point.
#' @param maxAngle A number giving the maximum acceptable angle, in radians,
#'   between consecutive knot-to-knot step vectors on either side of the seed
#'   knot; see [newBSplineTractFromStreamlineWithConstraints()]. The default,
#'   `NULL`, disables trimming.
#' @return A list with elements `spline`, the fitted [BSplineTract] object,
#'   and `options`, an updated copy of `options` with its `knotSpacing`
#'   element set to that used for the fit.
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
referenceSplineTractWithOptions <- function (options, refSession, refSeed, nStreamlines = 5000, maxAngle = NULL)
{
    refOptions <- createTractOptionList(options$pointType, options$lengthQuantile, FALSE, NULL, options$maxPathLength)
    streamline <- streamlineTractWithOptions(refOptions, refSession, refSeed, nStreamlines=nStreamlines)
    refSpline <- newBSplineTractFromStreamlineWithConstraints(streamline, maxAngle=maxAngle)
    
    options$knotSpacing <- refSpline$getKnotSpacing()
    
    invisible (list(spline=refSpline, options=options))
}

#' Generate candidate B-spline tracts across a seed neighbourhood
#'
#' This function generates a [BSplineTract] for each seed point in a
#' neighbourhood, skipping points that lie outside the image or (except at
#' the neighbourhood's centre) have fractional anisotropy below a threshold.
#' The direction used to disambiguate asymmetric tracking is taken from the
#' reference tract's own step vectors.
#'
#' @param session The [tractor.session::MriSession] to generate candidate
#'   tracts within.
#' @param neighbourhood A neighbourhood information object, as created by
#'   [tractor.base::createNeighbourhoodInfo()], giving the seed points to use.
#' @param reference A [ReferenceTract] object giving the reference tract that
#'   candidates will ultimately be compared against.
#' @param faThreshold A number giving the minimum fractional anisotropy
#'   required at a seed point (other than the neighbourhood's centre) for a
#'   candidate tract to be generated there.
#' @param nStreamlines The number of streamlines to generate from each seed
#'   point.
#' @return A list of [BSplineTract] objects (or `NA` for skipped or
#'   unfittable seed points), one per seed point in `neighbourhood`, returned
#'   invisibly.
#' @author Jon Clayden
#' @export
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

#' Calculate PNT posterior probabilities from a fitted matching model
#'
#' This function calculates, for each session represented in a PNT data
#' table, the posterior probability that each of its candidate tracts is the
#' correct match for the reference tract, under the given tract-matching
#' model, assuming a uniform prior over valid candidates.
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()].
#'   If it has no `sessionPath` column, all rows are assumed to belong to a
#'   single, unnamed session.
#' @param matchingModel A fitted [MatchingTractModel] object.
#' @return A list with elements `tp`, the per-session list of tract
#'   posteriors, `np`, the per-session list of null posteriors, and `mm`, the
#'   `matchingModel` argument unchanged. This is the form expected by
#'   [newProbabilisticNTResultsFromPosteriors()].
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

#' Fit a tract-matching model by Expectation-Maximisation
#'
#' This function jointly fits a [MatchingTractModel] and an
#' [UninformativeTractModel] to a PNT data table, and calculates posterior
#' probabilities for each candidate tract, by Expectation-Maximisation (EM).
#' At each iteration, the models are refitted using the current posterior
#' probabilities as weights (the M step), and the posteriors are then
#' recalculated from the refitted models (the E step). The algorithm
#' terminates when the log-evidence and the matching model's alpha parameters
#' have both converged.
#'
#' @param data A PNT data table, as created by [createDataTableForSplines()],
#'   which must include a `sessionPath` column identifying the session that
#'   each candidate tract belongs to.
#' @param refSpline A [BSplineTract] object giving the reference tract.
#' @param lengthCutoff An integer giving the maximum tract length, in points,
#'   to allow for. The default, `NULL`, uses the maximum length observed in
#'   `data`.
#' @param lambda A regularisation parameter passed to
#'   [fitRegularisedBetaDistribution()] when fitting the matching model's
#'   cosine distributions. The default, `NULL`, disables regularisation.
#' @param alphaOffset A number added to each fitted alpha parameter of the
#'   matching model after regularisation; see
#'   [fitRegularisedBetaDistribution()].
#' @param nullPrior A number giving the prior probability, for each session,
#'   that none of its candidate tracts is a correct match. The default,
#'   `NULL`, uses a data-dependent prior which favours a match, but becomes
#'   more conservative as the number of valid candidates increases.
#' @param asymmetricModel Boolean value: if `TRUE`, separate cosine
#'   distributions are fitted for the left and right sides of the seed in the
#'   matching model; otherwise data from both sides are pooled at each point.
#' @return A list with elements `tp`, the per-session list of tract
#'   posteriors, `np`, the per-session list of null posteriors, `mm`, the
#'   final fitted [MatchingTractModel], and `um`, the final fitted
#'   [UninformativeTractModel]. This is the form expected by
#'   [newProbabilisticNTResultsFromPosteriors()].
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#'
#' For the EM-based model fitting procedure specifically, see
#'
#' J.D. Clayden, A.J. Storkey, S. Muñoz Maniega & M.E. Bastin (2009).
#' Reproducibility of tract segmentation between sessions using an
#' unsupervised modelling-based approach. NeuroImage 45(2):377-385.
#' @export
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

#' Calculate posterior probabilities from log-likelihoods
#'
#' This function calculates, for a single session, the posterior
#' probabilities of each candidate tract being the correct match, and of none
#' of them being a match, from their log-likelihoods under a matching model
#' and under an alternative (typically uninformative) model, together with
#' their prior probabilities. Numerical overflow is handled by working with
#' log-likelihoods relative to the best-supported candidate.
#'
#' @param matchedLogLikelihoods A numeric vector of per-candidate
#'   log-likelihoods under the matching model.
#' @param nonmatchedLogLikelihoods A numeric vector, of the same length as
#'   `matchedLogLikelihoods`, of per-candidate log-likelihoods under the
#'   alternative model.
#' @param tractPriors A numeric vector, of the same length as
#'   `matchedLogLikelihoods`, of prior probabilities for each candidate being
#'   the correct match.
#' @param nullPrior A number giving the prior probability that none of the
#'   candidates is a correct match.
#' @return A list with elements `tractPosteriors`, a numeric vector of
#'   per-candidate posterior probabilities, `nullPosterior`, the posterior
#'   probability that no candidate matches, and `logEvidence`, the log model
#'   evidence for this session.
#' @author Jon Clayden
#' @export
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
