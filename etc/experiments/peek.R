#@args file name
#@desc Attempt to deserialise the R object stored in the file name specified (to which
#@desc the suffix ".Rdata" will be added if it's not already present). If successful,
#@desc the class of the stored object will be displayed, along with a summary of the
#@desc properties of the object if available.

suppressPackageStartupMessages(require(tractor.camino))
suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("file name")
    fileName <- ensureFileSuffix(Arguments[1], "Rdata")
    
    if (!file.exists(fileName))
        output(OL$Error, "File ", fileName, " does not exist")
    
    setOutputLevel(OL$Info, FALSE)
    
    object <- deserialiseListObject(file=fileName, raw=TRUE)
    if (!is.null(attr(object,"originalClass")))
        output(OL$Info, "Object class: ", implode(attr(object,"originalClass"),", "))
    
    if (isDeserialisable(object, "metatract.reference"))
        object <- deserialiseReferenceTract(NULL, object)
    else if (isDeserialisable(object, "results.nt.heuristic"))
        object <- deserialiseHeuristicNTResults(NULL, object)
    else if (isDeserialisable(object, "results.nt.probabilistic"))
        object <- deserialiseProbabilisticNTResults(NULL, object)
    else if (isDeserialisable(object, "model.tract.matching"))
        object <- deserialiseMatchingTractModel(NULL, object)
    else
        output(OL$Error, "Unknown class - cannot deserialise")
    
    if (!is.null(object$summarise))
    {
        cat("---\n")
        object$summarise()
    }
}
