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

writeNTResource <- function (object, type, options = NULL)
{
    fileName <- getFileNameForNTResource(type, options, intent="write")
    object$serialise(file=fileName)
}
