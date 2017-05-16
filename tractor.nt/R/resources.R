getFileNameForNTResource <- function (type, mode, options = NULL, intent = c("read","write"))
{
    validTypes <- list(hnt=c("reference","results"), pnt=c("reference","model","results"))
    
    mode <- match.arg(tolower(mode), c("hnt","pnt"))
    type <- match.arg(tolower(type), validTypes[[mode]])
    intent <- match.arg(intent)
    
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (tractorHome == "" || !file.exists(tractorHome))
        standardRefTractDir <- NULL
    else
        standardRefTractDir <- file.path(tractorHome, "share", "tractor", mode, "reftracts")
    
    if (type == "reference")
    {
        if (!("tractName" %in% names(options)))
            report(OL$Error, "Tract name must be specified")
            
        fileName <- ensureFileSuffix(paste(options$tractName,"ref",sep="_"), "Rdata")
        if (intent == "write" || file.exists(fileName))
            return (fileName)
        else if (file.exists(file.path(standardRefTractDir, fileName)))
            return (file.path(standardRefTractDir, fileName))
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
        else if (intent == "read" && mode == "hnt")
        {
            fileNames <- list.files(dirname(fileName))
            resultsStem <- ensureFileSuffix(fileName, NULL, strip="Rdata")
            match <- ore.search(ore("^",ore.escape(resultsStem),"\\.(\\d+)\\.Rdata$"), fileNames, simplify=FALSE)
            indices <- as.integer(groups(match, simplify=FALSE))
            fileNames <- fileNames[!is.na(indices)]
            indices <- indices[!is.na(indices)]
            objects <- lapply(fileNames[order(indices)], deserialiseReferenceObject)
            results <- Reduce(c, lapply(objects, function(x) x$getResults()))
            sessionPaths <- Reduce(c, lapply(objects, function(x) x$getSessionPaths()))
            finalObject <- HeuristicNTResults$new(results, sessionPaths)
            finalObject$serialise(fileName)
            unlink(fileNames)
            return (fileName)
        }
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
        else if (!any(c("datasetName","tractName") %in% names(options)))
            report(OL$Error, "Model or dataset and tract names must be specified")
        
        fileName <- ensureFileSuffix(paste(options$datasetName,"model",sep="_"), "Rdata")
        if (intent == "write" || file.exists(fileName))
            return (fileName)
        
        fileName <- ensureFileSuffix(paste(options$tractName,"model",sep="_"), "Rdata")
        if (file.exists(fileName))
            return (fileName)
        else
            report(OL$Error, "No suitable model was found")
    }
}

getNTResource <- function (type, mode, options = NULL)
{
    fileName <- getFileNameForNTResource(type, mode, options, intent="read")
    
    type <- tolower(type)
    mode <- tolower(mode)
    
    if (type == "reference")
    {
        reference <- deserialiseReferenceObject(fileName)
        if ((mode == "hnt" && !is(reference$getTract(),"FieldTract")) || (mode == "pnt" && !is(reference$getTract(),"BSplineTract")))
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

writeNTResource <- function (object, type, mode, options = NULL)
{
    fileName <- getFileNameForNTResource(type, mode, options, intent="write")
    object$serialise(file=fileName)
}
