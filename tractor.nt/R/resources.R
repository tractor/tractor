#' Locate the file for a PNT resource
#'
#' This function determines the file name (with an ".Rdata" extension) for a
#' probabilistic neighbourhood tractography (PNT) reference tract, matching
#' model or results object, checking the working directory and, for
#' reference tracts and models, TractoR's standard resource directories under
#' `TRACTOR_HOME`.
#'
#' For `type` `"model"`, several naming schemes are tried in turn: a
#' `modelName` element of `options`, used directly; a `datasetName` element,
#' suffixed with `"_model"`; and a `tractName` element, suffixed with
#' `"_model"` (this last option also being checked against the standard
#' model directory when `intent` is `"read"`).
#'
#' @param type A string, one of `"reference"`, `"model"` or `"results"`,
#'   giving the type of resource required.
#' @param options A named list, generally including a `tractName` element
#'   (for `"reference"` and `"model"` resources), a `resultsName` element
#'   (for `"results"` resources), a `datasetName` element, and/or a
#'   `modelName` element, used to construct the file name (see Details).
#' @param intent A string, either `"read"` (the default) or `"write"`,
#'   indicating whether the file is required to already exist.
#' @return A string giving the path to the resource file. An error is
#'   signalled if the file could not be found and `intent` is `"read"`.
#' @author Jon Clayden
#' @export
getFileNameForNTResource <- function (type, options = NULL, intent = c("read","write"))
{
    type <- match.arg(tolower(type), c("reference","model","results"))
    intent <- match.arg(intent)
    
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (tractorHome == "" || !file.exists(tractorHome))
        standardRefTractDir <- standardModelDir <- NULL
    else
    {
        standardRefTractDir <- file.path(tractorHome, "share", "tractor", "pnt", "reftracts")
        standardModelDir <- file.path(tractorHome, "share", "tractor", "pnt", "models")
    }
    
    refTractSubDir <- tolower(Sys.getenv("TRACTOR_REFTRACT_SET"))
    refTractSubDir <- ifelse(refTractSubDir %in% c("miua2017","ismrm2008"), refTractSubDir, "ismrm2008")
    
    if (type == "reference")
    {
        if (!("tractName" %in% names(options)))
            report(OL$Error, "Tract name must be specified")
            
        fileName <- ensureFileSuffix(paste(options$tractName,"ref",sep="_"), "Rdata")
        if (intent == "write" || file.exists(fileName))
            return (fileName)
        else if (file.exists(file.path(standardRefTractDir, refTractSubDir, fileName)))
            return (file.path(standardRefTractDir, refTractSubDir, fileName))
        else
            report(OL$Error, "No reference for tract name \"#{options$tractName}\" was found")
    }
    else if (type == "results")
    {
        if (!("resultsName" %in% names(options)))
            report(OL$Error, "Results name must be specified")
        
        fileName <- ensureFileSuffix(options$resultsName, "Rdata")
        if (intent == "write" || file.exists(fileName))
            return (fileName)
        else
            report(OL$Error, "No results file with name \"#{options$resultsName}\" was found")
    }
    else if (type == "model")
    {
        if ("modelName" %in% names(options))
        {
            fileName <- ensureFileSuffix(options$modelName, "Rdata")
            if (intent == "write" || file.exists(fileName))
                return (fileName)
        }
        if ("datasetName" %in% names(options))
        {
            fileName <- ensureFileSuffix(paste(options$datasetName,"model",sep="_"), "Rdata")
            if (intent == "write" || file.exists(fileName))
                return (fileName)
        }
        if ("tractName" %in% names(options))
        {
            fileName <- ensureFileSuffix(paste(options$tractName,"model",sep="_"), "Rdata")
            if (file.exists(fileName))
                return (fileName)
            else if (intent == "read" && file.exists(file.path(standardModelDir, fileName)))
                return (file.path(standardModelDir, fileName))
        }
        report(OL$Error, "No suitable model was found")
    }
}

#' Read a PNT resource from file
#'
#' This function locates, using [getFileNameForNTResource()], and deserialises
#' a probabilistic neighbourhood tractography (PNT) reference tract, matching
#' model or results object.
#'
#' @param type A string, one of `"reference"`, `"model"` or `"results"`,
#'   giving the type of resource required.
#' @param options A named list of elements used to construct the resource's
#'   file name; see [getFileNameForNTResource()].
#' @return The deserialised resource object: a [ReferenceTract] for
#'   `type="reference"`, an [UninformativeTractModel] or [MatchingTractModel]
#'   for `type="model"`, or a [ProbabilisticNTResults] object for
#'   `type="results"`. The result is returned invisibly.
#' @author Jon Clayden
#' @export
getNTResource <- function (type, options = NULL)
{
    fileName <- getFileNameForNTResource(type, options, intent="read")
    
    type <- tolower(type)
    
    if (type == "reference")
    {
        reference <- deserialiseReferenceObject(fileName)
        if (!is(reference$getTract(), "BSplineTract"))
            report(OL$Error, "The specified reference tract is not in the correct form")
        else
            return (invisible(reference))
    }
    else
    {
        object <- deserialiseReferenceObject(fileName)
        return (invisible(object))
    }
}

#' Write a PNT resource to file
#'
#' This function serialises a probabilistic neighbourhood tractography (PNT)
#' reference tract, matching model or results object to file, at the location
#' determined by [getFileNameForNTResource()].
#'
#' @param object The object to serialise: usually a [ReferenceTract],
#'   [UninformativeTractModel], [MatchingTractModel] or
#'   [ProbabilisticNTResults] object.
#' @param type A string, one of `"reference"`, `"model"` or `"results"`,
#'   giving the type of resource being written.
#' @param options A named list of elements used to construct the resource's
#'   file name; see [getFileNameForNTResource()].
#' @return The serialised representation of `object`, as returned by
#'   [tractor.base::serialiseReferenceObject()], invisibly. This function is
#'   mainly called for its side effect of writing the file.
#' @author Jon Clayden
#' @export
writeNTResource <- function (object, type, options = NULL)
{
    fileName <- getFileNameForNTResource(type, options, intent="write")
    object$serialise(file=fileName)
}
