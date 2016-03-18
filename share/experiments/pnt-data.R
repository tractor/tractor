#@desc Create B-spline tract representations and calculate characteristics of interest for a set of seed points in one or more brain volumes. This is a prerequisite for training or using a tract matching model. For training, a specific list of seed points will likely be required, in which case PointType should also be set. For using a model, the SeedPointList need not be given, in which case a region of width SearchWidth voxels around the reference tract seed point will be used, subject to the specified AnisotropyThreshold. The TractName specified must match that given to the "pnt-ref" experiment. If the SessionNumbers option is used to select a subset of the data sets, manual collation using "pnt-collate" will be required.
#@args session directory, [seed point]

library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE)
    searchWidth <- getConfigVariable("SearchWidth", 1)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.2)
    nStreamlines <- getConfigVariable("Streamlines", 1000)
    datasetName <- getConfigVariable("DatasetName", "data")
    overwrite <- getConfigVariable("Overwrite", FALSE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    requireArguments("session directory")
    
    session <- attachMriSession(Arguments[1])
    
    if (isValidAs(Sys.getenv("TRACTOR_PLOUGH_ID"), "integer"))
        fileName <- ensureFileSuffix(paste(datasetName,Sys.getenv("TRACTOR_PLOUGH_ID"),sep="."), "txt")
    else
        fileName <- ensureFileSuffix(datasetName, "txt")
    
    if (file.exists(fileName) && !overwrite)
        report(OL$Info, "Dataset file exists - not overwriting it")
    else
    {
        if (nArguments() > 1)
        {
            seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
            if (is.null(pointType))
                report(OL$Error, "Point type must be specified with the seed point")
            seed <- round(changePointType(seed, session$getRegistrationTarget("diffusion",metadataOnly=TRUE), "r", pointType))
        }
        else
            seed <- transformPointsToSpace(reference$getStandardSpaceSeedPoint(), session, "diffusion", oldSpace="mni", pointType=reference$getSeedUnit(), outputVoxel=TRUE, nearest=TRUE)
        
        neighbourhood <- createNeighbourhoodInfo(centre=seed, width=searchWidth)
        splines <- calculateSplinesForNeighbourhood(session, neighbourhood, reference, faThreshold, nStreamlines)
        data <- createDataTableForSplines(splines, reference$getTract(), "knot", subjectId=i, neighbourhood=neighbourhood)
        write.table(data, file=fileName)
    }
}
