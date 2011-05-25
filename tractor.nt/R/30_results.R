HeuristicNTResults <- setRefClass("HeuristicNTResults", contains="SerialisableObject", fields=list(results="list"), methods=list(
    getResult = function (pos) { return (results[[pos]]) },
    
    nSessions = function () { return (length(results)) },
    
    summarise = function ()
    {
        report(OL$Info, "Number of sessions: ", .self$nSessions())
    }
))

setClassUnion("UninformativeTractModelOrNull", c("UninformativeTractModel","NULL"))

ProbabilisticNTResults <- setRefClass("ProbabilisticNTResults", contains="SerialisableObject", fields=list(tractPosteriors="list",nullPosteriors="list",matchingModel="MatchingTractModel",uninformativeModel="UninformativeTractModelOrNull"), methods=list(
    getMatchingModel = function () { return (matchingModel) },
    
    getNullPosterior = function (pos = NULL)
    {
        if (is.null(pos))
            return (nullPosterior)
        else
            return (nullPosterior[[pos]])
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
        if (length(tractPosteriors) == 0)
            return (0)
        else
            return (length(tractPosteriors[[1]]))
    },
    
    nSessions = function () { return (length(tractPosteriors)) },
    
    summarise = function ()
    {
        report(OL$Info, "Number of sessions: ", .self$nSessions())
        report(OL$Info, "Seeds per session : ", .self$nPoints())
    }
))

newHeuristicNTResultsFromList <- function (results)
{
    if (!is.list(results) || length(results) == 0 || !is.list(results[[1]]))
        report(OL$Error, "The specified heuristic NT results list appears to be invalid")
    
    object <- HeuristicNTResults$new(results=results)
    invisible (object)
}

newProbabilisticNTResultsFromPosteriors <- function (tractPosteriors, nullPosteriors, matchingModel, uninformativeModel = NULL)
{
    if (!is.list(tractPosteriors) || !is.list(nullPosteriors) || !is(matchingModel,"MatchingTractModel"))
        report(OL$Error, "Some of the probabilistic NT results information provided is invalid")
    if (!is.null(uninformativeModel) && !is(uninformativeModel,"UninformativeTractModel"))
        report(OL$Error, "The specified uninformative model is not an UninformativeTractModel object")
    
    object <- ProbabilisticNTResults$new(tractPosteriors=tractPosteriors, nullPosteriors=nullPosteriors, matchingModel=matchingModel, uninformativeModel=uninformativeModel)
    invisible (object)
}
