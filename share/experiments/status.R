#@args [session directory]
#@desc Print the status of a session directory (default "."), including various information about what processes have been run on it.
#@nohistory TRUE

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    report(OL$Info, "GENERAL:", prefixFormat="")
    printLabelledValues(c("Session directory","Working directory exists"), c(session$getDirectory(),file.exists(session$getDirectory("root"))), leftJustify=TRUE)
    
    if (file.exists(session$getDirectory("diffusion")))
    {
        report(OL$Info, "\nDIFFUSION:", prefixFormat="")
        labels <- c("Preprocessing complete", "Data dimensions", "Voxel dimensions", "Diffusion b-values", "Number of gradient directions", "Diffusion tensors fitted", "FSL BEDPOST run", "Camino files created")
        if (imageFileExists(session$getImageFileNameByType("data","diffusion")))
        {
            metadata <- session$getImageByType("data","diffusion",metadataOnly=TRUE)
            metadataSummary <- metadata$summarise()
            dims <- metadataSummary$values[2]
            voxelDims <- metadataSummary$values[3]
        }
        else
            dims <- voxelDims <- NA
        
        nFibres <- getBedpostNumberOfFibresForSession(session)
        if (nFibres == 0)
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
        values <- c(imageFileExists(session$getImageFileNameByType("data","diffusion")), dims, voxelDims, bValues, directions, imageFileExists(session$getImageFileNameByType("fa","diffusion")), bedpostValue, file.exists(file.path(session$getDirectory("camino"),"sequence.scheme")))
        printLabelledValues(labels, values, leftJustify=TRUE)
    }
}
