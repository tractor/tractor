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
        if (session$imageExists("data","diffusion"))
        {
            metadata <- session$getImageByType("data", "diffusion", metadataOnly=TRUE)
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
        
        values <- c(session$imageExists("data","diffusion"), dims, voxelDims, bValues, directions, session$imageExists("FA","diffusion"), bedpostValue, file.exists(file.path(session$getDirectory("camino"),"sequence.scheme")))
        printLabelledValues(labels, values, leftJustify=TRUE)
    }
    
    if (file.exists(session$getDirectory("functional")))
    {
        report(OL$Info, "\nFUNCTIONAL:", prefixFormat="")
        labels <- c("Data dimensions", "Voxel dimensions", "Sampling frequency", "Total time")
        
        if (session$imageExists("data","functional"))
        {
            metadata <- session$getImageByType("data", "functional", metadataOnly=TRUE)
            metadataSummary <- metadata$summarise()
            dims <- metadataSummary$values[2]
            voxelDims <- metadataSummary$values[3]
            frequency <- es("#{1/metadata$getVoxelDimensions()[4]} Hz", signif=3)
            
            timeSeconds <- metadata$getVoxelDimensions()[4] * metadata$getDimensions()[4]
            if (!is.finite(timeSeconds))
                time <- NA
            else if (timeSeconds > 60)
                time <- es("#{timeSeconds/60} min", signif=3)
            else
                time <- es("#{timeSeconds} s", signif=3)
        }
        else
            dims <- voxelDims <- frequency <- time <- NA
        
        values <- c(dims, voxelDims, frequency, time)
        printLabelledValues(labels, values, leftJustify=TRUE)
    }
    
    if (file.exists(session$getDirectory("structural")))
    {
        report(OL$Info, "\nSTRUCTURAL:", prefixFormat="")
        labels <- c("Number of T1w volumes", "T1w space dimensions", "T1w voxel dimensions", "Parcellation created", "Number of T2w volumes", "Number of PDw volumes")
        
        t1Count <- getImageCountForSession(session, "t1", "structural")
        t2Count <- getImageCountForSession(session, "t2", "structural")
        pdCount <- getImageCountForSession(session, "pd", "structural")
        
        if (t1Count > 0)
        {
            if (session$imageExists("refT1", "structural"))
                metadata <- session$getImageByType("refT1", "structural", metadataOnly=TRUE)
            else
                metadata <- session$getImageByType("t1", "structural", index=1, metadataOnly=TRUE)
            
            metadataSummary <- metadata$summarise()
            dims <- metadataSummary$values[2]
            voxelDims <- metadataSummary$values[3]
        }
        else
            dims <- voxelDims <- NA
        
        values <- c(t1Count, dims, voxelDims, session$imageExists("parcellation","structural"), t2Count, pdCount)
        printLabelledValues(labels, values, leftJustify=TRUE)
    }
}
