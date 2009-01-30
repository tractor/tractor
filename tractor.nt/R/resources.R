getFileNameForNTResource <- function (type, mode, options = NULL, expectExists = FALSE)
{
    validTypes <- list(hnt=c("reference","results"), pnt=c("reference","model","results"))
    
    mode <- match.arg(tolower(mode), c("hnt","pnt"))
    type <- match.arg(tolower(type), validTypes[[mode]])
    
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (tractorHome == "" || !file.exists(tractorHome))
        standardRefTractDir <- NULL
    else
        standardRefTractDir <- file.path(tractorHome, "share", "reftracts", mode)
    
    if (type == "reference")
    {
        if (!("tractName" %in% names(options)))
            output(OL$Error, "Tract name must be specified")
            
        fileName <- ensureFileSuffix(paste(options$tractName,"ref",sep="_"), "Rdata")
        if (!expectExists || file.exists(fileName))
            return (fileName)
        else if (file.exists(file.path(standardRefTractDir, fileName)))
            return (file.path(standardRefTractDir, fileName))
        else
            output(OL$Error, "No reference for tract name \"", options$tractName, "\" was found")
    }
    else if (type == "results")
    {
        if (!("resultsName" %in% names(options)))
            output(OL$Error, "Results name must be specified")
        
        fileName <- ensureFileSuffix(options$resultsName, "Rdata")
        if (!expectExists || file.exists(fileName))
            return (fileName)
        else
            output(OL$Error, "No results file with name \"", options$resultsName, "\" was found")
    }
    else if (type == "model")
    {
        if (!any(c("datasetName","tractName") %in% names(options)))
            output(OL$Error, "Dataset and tract names must be specified")
        
        fileName <- ensureFileSuffix(paste(options$datasetName,"model",sep="_"), "Rdata")
        if (!expectExists || file.exists(fileName))
            return (fileName)
        
        fileName <- ensureFileSuffix(paste(options$tractName,"model",sep="_"), "Rdata")
        if (file.exists(fileName))
            return (fileName)
        else
            output(OL$Error, "No suitable model was found")
    }
}

getNTResource <- function (type, mode, options = NULL)
{
    fileName <- getFileNameForNTResource(type, mode, options, expectExists=TRUE)
    
    type <- tolower(type)
    mode <- tolower(mode)
    
    if (type == "reference")
    {
        reference <- deserialiseReferenceTract(fileName)
        if ((mode == "hnt" && !isFieldTract(reference)) || (mode == "pnt" && !isBSplineTract(reference)))
            output(OL$Error, "The specified reference tract is not in the correct form")
        else
            return (invisible(reference))
    }
    else if (type == "results")
    {
        deserialiseFunction <- match.fun(ifelse(mode=="hnt", "deserialiseHeuristicNTResults", "deserialiseProbabilisticNTResults"))
        results <- deserialiseFunction(fileName)
        return (invisible(results))
    }
    else if (type == "model")
    {
        model <- deserialiseMatchingTractModel(fileName)
        return (invisible(model))
    }
}

writeNTResource <- function (object, type, mode, options = NULL)
{
    fileName <- getFileNameForNTResource(type, mode, options)
    serialiseListObject(object, file=fileName)
}
