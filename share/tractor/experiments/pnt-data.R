#@desc Create B-spline tract representations and calculate characteristics of interest for a set of seed points in a brain volume. This is a prerequisite for training or using a tract matching model. For training, a specific seed point (using the R voxel convention) will likely be required. For using a model, the a seed need not be given, in which case a region of width SearchWidth voxels around the reference tract seed point will be used, subject to the specified AnisotropyThreshold. The TractName specified must match that given to the "pnt-ref" experiment. The old SessionList option can be specified to update the output from the TractoR 2.x version of this script.
#@args session directory, [seed point]
#@group Neighbourhood tractography

library(tractor.track)
library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    searchWidth <- getConfigVariable("SearchWidth", 1)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.2)
    nStreamlines <- getConfigVariable("Streamlines", 1000)
    datasetName <- getConfigVariable("DatasetName", "data")
    overwrite <- getConfigVariable("Overwrite", FALSE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", deprecated=TRUE)
    
    # For backwards compatibility: update an old dataset file
    if (!is.null(sessionList))
    {
        assert(overwrite, "The SessionList variable is only used to update old files - also set Overwrite:true if you want to do this")
        
        fileName <- ensureFileSuffix(datasetName, "txt")
        data <- read.table(fileName)
        
        if (!("subject" %in% colnames(data)))
            report(OL$Error, "Subject number must be stored in the old dataset")
        index <- which(colnames(data) == "subject")
        data[,index] <- sessionList[data[,index]]
        colnames(data)[index] <- "sessionPath"
        
        write.table(data, file=fileName)
        return (invisible(NULL))
    }
    
    requireArguments("session directory")
    session <- attachMriSession(Arguments[1])
    
    reference <- getNTResource("reference", list(tractName=tractName))
    
    if (isValidAs(Sys.getenv("TRACTOR_PLOUGH_ID"), "integer"))
        fileName <- ensureFileSuffix(paste(datasetName,Sys.getenv("TRACTOR_PLOUGH_ID"),sep="."), "txt")
    else
        fileName <- ensureFileSuffix(datasetName, "txt")
    
    if (file.exists(fileName) && !overwrite)
        report(OL$Info, "Dataset file exists - not overwriting it")
    else
    {
        if (nArguments() > 1)
            seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        else
            seed <- transformPointsToSpace(reference$getStandardSpaceSeedPoint(), session, "diffusion", oldSpace="mni", pointType=reference$getSeedUnit(), outputVoxel=TRUE, nearest=TRUE)
        
        neighbourhood <- createNeighbourhoodInfo(centre=seed, width=searchWidth)
        splines <- calculateSplinesForNeighbourhood(session, neighbourhood, reference, faThreshold, nStreamlines)
        data <- createDataTableForSplines(splines, reference$getTract(), "knot", sessionPath=Arguments[1], neighbourhood=neighbourhood)
        write.table(data, file=fileName)
    }
}
