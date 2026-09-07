setClassUnion("UninformativeTractModelOrNull", c("UninformativeTractModel","NULL"))

#' The ProbabilisticNTResults class
#'
#' This class represents the results of applying probabilistic neighbourhood
#' tractography (PNT) to one or more sessions: for each session, the
#' posterior probability that each candidate tract is the correct match for
#' the reference tract, together with the posterior probability that none of
#' the candidates match (the "null" posterior). Objects of this class are
#' usually created by [newProbabilisticNTResultsFromPosteriors()], and are
#' the result of [calculatePosteriorsForDataTable()] or
#' [runMatchingEMForDataTable()].
#'
#' @field tractPosteriors A list, one element per session, each a numeric
#'   vector giving the posterior probability of each candidate tract being
#'   the correct match, under `matchingModel`.
#' @field nullPosteriors A list, one element per session, each a single
#'   number giving the posterior probability that no candidate is a correct
#'   match.
#' @field matchingModel A [MatchingTractModel] object giving the model used
#'   to calculate `tractPosteriors` and `nullPosteriors`.
#' @field uninformativeModel An optional [UninformativeTractModel] object
#'   giving the alternative, uninformative model used alongside
#'   `matchingModel` when the posteriors were calculated, or `NULL` if this
#'   is not applicable or not known.
#'
#' @export
ProbabilisticNTResults <- setRefClass("ProbabilisticNTResults", contains="SerialisableObject", fields=list(tractPosteriors="list",nullPosteriors="list",matchingModel="MatchingTractModel",uninformativeModel="UninformativeTractModelOrNull"), methods=list(
    getMatchingModel = function () { return (matchingModel) },
    
    getNullPosterior = function (pos = NULL)
    {
        if (is.null(pos))
            return (nullPosteriors)
        else
            return (nullPosteriors[[pos]])
    },
    
    getTractPosteriors = function (pos = NULL)
    {
        if (is.null(pos))
            return (tractPosteriors)
        else
            return (tractPosteriors[[pos]])
    },
    
    getUninformativeModel = function () { return (uninformativeModel) },
    
    nPoints = function ()
    {
        "Return the number of candidate seed points (tracts) per session"
        if (length(tractPosteriors) == 0)
            return (0)
        else
            return (length(tractPosteriors[[1]]))
    },
    
    nSessions = function () { return (length(tractPosteriors)) },
    
    summarise = function ()
    { 
        "Summarise the PNT results"
        labels <- c("Number of sessions", "Seeds per session")
        values <- c(.self$nSessions(), .self$nPoints())
        return (list(labels=labels, values=values))
    }
))

#' Create a set of PNT results
#'
#' This function creates a [ProbabilisticNTResults] object encapsulating the
#' final output of probabilistic neighbourhood tractography (PNT): the
#' posterior probabilities that each candidate tract, in each session, is a
#' correct match for the reference tract, and that no candidate is a match.
#' These are usually the direct output of [calculatePosteriorsForDataTable()]
#' or [runMatchingEMForDataTable()].
#'
#' @param tractPosteriors A list, one element per session, each a numeric
#'   vector giving the posterior probability of each candidate tract being
#'   the correct match.
#' @param nullPosteriors A list, one element per session, each a single
#'   number giving the posterior probability that no candidate is a correct
#'   match.
#' @param matchingModel A [MatchingTractModel] object giving the model used
#'   to calculate the posteriors.
#' @param uninformativeModel An optional [UninformativeTractModel] object
#'   giving the alternative, uninformative model used alongside
#'   `matchingModel`. The default, `NULL`, indicates that this information is
#'   not available or not applicable.
#' @return A [ProbabilisticNTResults] object.
#' @author Jon Clayden
#' @export
newProbabilisticNTResultsFromPosteriors <- function (tractPosteriors, nullPosteriors, matchingModel, uninformativeModel = NULL)
{
    if (!is.list(tractPosteriors) || !is.list(nullPosteriors) || !is(matchingModel,"MatchingTractModel"))
        report(OL$Error, "Some of the probabilistic NT results information provided is invalid")
    if (!is.null(uninformativeModel) && !is(uninformativeModel,"UninformativeTractModel"))
        report(OL$Error, "The specified uninformative model is not an UninformativeTractModel object")
    
    object <- ProbabilisticNTResults$new(tractPosteriors=tractPosteriors, nullPosteriors=nullPosteriors, matchingModel=matchingModel, uninformativeModel=uninformativeModel)
    invisible (object)
}
