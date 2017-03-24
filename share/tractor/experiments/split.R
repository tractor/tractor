#@args file name
#@desc Split an ".Rdata" file containing multiple objects into several files containing one object each.

runExperiment <- function ()
{
    requireArguments("file name")
    fileName <- ensureFileSuffix(Arguments[1], "Rdata")
    fileStem <- ensureFileSuffix(fileName, NULL, strip="Rdata")
    
    if (!file.exists(fileName))
        report(OL$Error, "File ", fileName, " does not exist")
    
    object <- deserialiseReferenceObject(fileName)
    
    if (!is.null(attr(object,"originalClass")))
        report(OL$Error, "The specified file does not seem to contain multiple objects")
    
    for (i in seq_along(object))
        object[[i]]$serialise(paste(fileStem, i, sep="_"))
}
