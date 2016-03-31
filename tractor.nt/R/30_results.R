HeuristicNTResults <- setRefClass("HeuristicNTResults", contains="SerialisableObject", fields=list(results="list",sessionPaths="character"), methods=list(
    initialize = function (results = list(), sessionPaths = character(0), ...)
    {
        if (length(sessionPaths) > 0 && length(sessionPaths) != length(results))
            report(OL$Error, "Session path length does not match the results list")
        
        initFields(results=result, sessionPaths=sessionPaths)
    },
    
    getResult = function (pos) { return (results[[pos]]) },
    
    getResults = function () { return (results) },
    
    getSessionPaths = function () { return (sessionPaths) },
    
    nSessions = function () { return (length(results)) },
    
    summarise = function () { return (list(labels="Number of sessions", values=.self$nSessions())) }
))

setClassUnion("UninformativeTractModelOrNull", c("UninformativeTractModel","NULL"))

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
        if (length(tractPosteriors) == 0)
            return (0)
        else
            return (length(tractPosteriors[[1]]))
    },
    
    nSessions = function () { return (length(tractPosteriors)) },
    
    summarise = function ()
    { 
        labels <- c("Number of sessions", "Seeds per session")
        values <- c(.self$nSessions(), .self$nPoints())
        return (list(labels=labels, values=values))
    }
))

newProbabilisticNTResultsFromPosteriors <- function (tractPosteriors, nullPosteriors, matchingModel, uninformativeModel = NULL)
{
    if (!is.list(tractPosteriors) || !is.list(nullPosteriors) || !is(matchingModel,"MatchingTractModel"))
        report(OL$Error, "Some of the probabilistic NT results information provided is invalid")
    if (!is.null(uninformativeModel) && !is(uninformativeModel,"UninformativeTractModel"))
        report(OL$Error, "The specified uninformative model is not an UninformativeTractModel object")
    
    object <- ProbabilisticNTResults$new(tractPosteriors=tractPosteriors, nullPosteriors=nullPosteriors, matchingModel=matchingModel, uninformativeModel=uninformativeModel)
    invisible (object)
}
