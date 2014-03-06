#@args file name
#@desc Attempt to deserialise the R object stored in the file name specified (to which the suffix ".Rdata" will be added if it's not already present). If successful, the class of the stored object will be displayed, along with a summary of the properties of the object if available.
#@nohistory TRUE

library(tractor.reg)
library(tractor.session)
library(splines)
library(tractor.nt)

runExperiment <- function ()
{
    requireArguments("file name")
    fileName <- ensureFileSuffix(Arguments[1], "Rdata")
    
    if (!file.exists(fileName))
        report(OL$Error, "File ", fileName, " does not exist")
    
    setOutputLevel(OL$Info)
    
    object <- deserialiseReferenceObject(file=fileName, raw=TRUE)
    if (is.null(attr(object,"originalClass")))
    {
        elementClasses <- sapply(object, attr, "originalClass")
        elementClasses <- paste("\"", elementClasses, "\"", sep="")
        report(OL$Info, "A list of #{length(object)} object(s) of class(es) #{implode(unique(elementClasses),', ')}", prefixFormat="")
    }
    else
    {
        report(OL$Info, "An object of class \"#{implode(attr(object,'originalClass'),', ')}\"", prefixFormat="")
        object <- deserialiseReferenceObject(object=object)
        cat("---\n")
        print(object)
    }
}
