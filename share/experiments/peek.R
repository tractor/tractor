#@args file name
#@desc Attempt to deserialise the R object stored in the file name specified (to which the suffix ".Rdata" will be added if it's not already present). If successful, the class of the stored object will be displayed, along with a summary of the properties of the object if available.
#@nohistory TRUE

library(tractor.reg)
library(tractor.session)
library(tractor.nt)
library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("file name")
    fileNames <- ensureFileSuffix(Arguments[1], c("Rdata","csv"))
    
    setOutputLevel(OL$Info)
    
    filesExist <- file.exists(fileNames)
    if (!any(filesExist))
        report(OL$Error, "No suitable file named #{Arguments[1]} exists")
    else if (!filesExist[1])
    {
        object <- readGraphFile(fileNames[2])
        report(OL$Info, "An object of class \"Graph\"", prefixFormat="")
        cat("---\n")
        print(object)
    }
    else
    {
        object <- deserialiseReferenceObject(file=fileNames[1], raw=TRUE)
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
}
