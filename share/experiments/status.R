#@args [session directory]
#@desc Print the status of a session directory (default "."), including various information about what processes have been run on it.

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    report(OL$Info, "GENERAL:", prefixFormat="")
    printLabelledValues(c("Session directory","Working directory exists"), c(session$getDirectory(),file.exists(session$getDirectory("root"))), leftJustify=TRUE)
    
    report(OL$Info, "\nDIFFUSION:", prefixFormat="")
    labels <- c("Preprocessing complete", "Diffusion b-values", "Number of gradient directions", "Diffusion tensors fitted", "FSL BEDPOST run", "Camino files created")
    if (is.na(nFibres <- getBedpostNumberOfFibresForSession(session)))
        bedpostValue <- FALSE
    else
        bedpostValue <- paste("TRUE (", nFibres, " fibre(s) per voxel)", sep="")
    if (is.null(scheme <- newSimpleDiffusionSchemeFromSession(session)))
        bValues <- directions <- NA
    else
    {
        bValues <- paste(implode(round(scheme$getBValues()),", "), "s/mm^2")
        directions <- implode(scheme$nDirections(), ", ")
    }
    values <- c(imageFileExists(session$getImageFileNameByType("mask","diffusion")), bValues, directions, imageFileExists(session$getImageFileNameByType("fa","diffusion")), bedpostValue, file.exists(file.path(session$getDirectory("camino"),"sequence.scheme")))
    printLabelledValues(labels, values, leftJustify=TRUE)
}
