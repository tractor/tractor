.HeuristicNTResults <- function (.results)
{
    self <- list(
        getResult = function (pos) { return (.results[[pos]]) },
        
        nSessions = function () { return (length(.results)) },
        
        summarise = function ()
        {
            output(OL$Info, "Number of sessions: ", self$nSessions())
        }
    )
    
    class(self) <- c("results.nt.heuristic", "list.object", "list")
    invisible (self)
}

.ProbabilisticNTResults <- function (.tractPosteriors, .nullPosterior, .matchingModel, .uninformativeModel)
{
    self <- list(
        getMatchingModel = function () { return (.matchingModel) },
        
        getNullPosterior = function () { return (.nullPosterior) },
        
        getTractPosteriors = function (pos = NULL)
        {
            if (is.null(pos))
                return (.tractPosteriors)
            else
                return (.tractPosteriors[[pos]])
        },
        
        getUninformativeModel = function () { return (.uninformativeModel) },
        
        nPoints = function ()
        {
            if (length(.tractPosteriors) == 0)
                return (0)
            else
                return (length(.tractPosteriors[[1]]))
        },
        
        nSessions = function () { return (length(.tractPosteriors)) },
        
        summarise = function ()
        {
            output(OL$Info, "Number of sessions: ", self$nSessions())
            output(OL$Info, "Seeds per session : ", self$nPoints())
        }
    )
    
    class(self) <- c("results.nt.probabilistic", "list.object", "list")
    invisible (self)
}

deserialiseHeuristicNTResults <- function (file = NULL, object = NULL)
{
    results <- deserialiseListObject(file, object, .HeuristicNTResults)
    invisible (results)
}

deserialiseProbabilisticNTResults <- function (file = NULL, object = NULL)
{
    if (is.null(object))
        object <- deserialiseListObject(file, raw=TRUE)
    
    if (isDeserialisable(object$matchingModel, "model.tract.matching"))
        object$matchingModel <- deserialiseMatchingTractModel(object=object$matchingModel)
    else
        output(OL$Error, "Deserialised object contains no valid matching model")
    
    if (isDeserialisable(object$uninformativeModel, "model.tract.uninformative"))
        object$uninformativeModel <- deserialiseUninformativeTractModel(object=object$uninformativeModel)
    
    results <- deserialiseListObject(NULL, object, .ProbabilisticNTResults)
    invisible (results)
}

newHeuristicNTResultsFromList <- function (results)
{
    if (!is.list(results) || length(results) == 0 || !is.list(results[[1]]))
        output(OL$Error, "The specified heuristic NT results list appears to be invalid")
    
    object <- .HeuristicNTResults(results)
    invisible (object)
}

newProbabilisticNTResultsFromPosteriors <- function (tractPosteriors, nullPosterior, matchingModel, uninformativeModel = NULL)
{
    if (!is.list(tractPosteriors) || !is.numeric(nullPosterior) || !isMatchingTractModel(matchingModel))
        output(OL$Error, "Some of the probabilistic NT results information provided is invalid")
    if (!is.null(uninformativeModel) && !isUninformativeTractModel(uninformativeModel))
        output(OL$Error, "The specified uninformative model is not an UninformativeTractModel object")
    
    results <- .ProbabilisticNTResults(tractPosteriors, nullPosterior, matchingModel, uninformativeModel)
    invisible (results)
}
