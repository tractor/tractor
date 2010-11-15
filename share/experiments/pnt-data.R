#@desc Create B-spline tract representations and calculate characteristics of interest
#@desc for a set of seed points in one or more brain volumes. This is a prerequisite
#@desc for training or using a tract matching model. For training, a specific list of
#@desc seed points will likely be required, in which case PointType should also be
#@desc set. For using a model, the SeedPointList need not be given, in which case a
#@desc region of width SearchWidth voxels around the reference tract seed point will
#@desc be used, subject to the specified AnisotropyThreshold. The TractName specified
#@desc must match that given to the "pnt-ref" experiment.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    tracker <- getWithDefault("Tracker", "fsl", validValues=c("fsl","tractor"))
    seedPoint <- getWithDefault("SeedPoint", NULL, "character")
    seedList <- getWithDefault("SeedPointList", NULL, "integer")
    pointType <- getWithDefault("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE)
    searchWidth <- getWithDefault("SearchWidth", 1)
    faThreshold <- getWithDefault("AnisotropyThreshold", 0.2)
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    datasetName <- getWithDefault("DatasetName", "data")
    sessionNumbers <- getWithDefault("SessionNumbers", NULL, "character")
    resume <- getWithDefault("Resume", FALSE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    if (!is.null(seedPoint) && !is.null(seedList))
        output(OL$Error, "Only one of \"SeedPoint\" and \"SeedPointList\" should be given")
    
    if (is.null(seedPoint) && is.null(seedList))
        pointType <- "r"
    else
    {
        if (is.null(pointType))
            output(OL$Error, "Point type must be specified with the seed point(s)")
        
        if (!is.null(seedList))
            seedMatrix <- matrix(seedList, ncol=3, byrow=TRUE)
        else
        {
            seedPoint <- splitAndConvertString(seedPoint, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
            seedMatrix <- promote(seedPoint, byrow=TRUE)
        }
        
        if (pointType == "fsl")
            seedMatrix <- transformFslVoxelToRVoxel(seedMatrix)
    }
    
    if (is.null(sessionNumbers))
        sessionNumbers <- seq_along(sessionList)
    else
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
    
    parallelApply(sessionNumbers, function (i) {
        sessionDatasetName <- ensureFileSuffix(paste(datasetName,"_session",i,sep=""), "txt")
        
        if (resume && file.exists(sessionDatasetName))
            output(OL$Info, "Using existing output for session ", i)
        else
        {
            output(OL$Info, "Calculating splines for session ", i)
            
            # Allow for out of bounds session numbers (used by pnt-data-sge)
            if (length(sessionList) == 1)
                currentSession <- newSessionFromDirectory(sessionList)
            else
                currentSession <- newSessionFromDirectory(sessionList[i])
        
            if (exists("seedMatrix"))
            {
                if (length(sessionList) == 1)
                    currentSeed <- drop(seedMatrix)
                else
                    currentSeed <- seedMatrix[i,]
            }
            else
                currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType=reference$getSeedUnit(), isStandard=TRUE)
        
            if (pointType == "mm")
                currentSeed <- transformWorldToRVoxel(currentSeed, newMriImageMetadataFromFile(currentSession$getImageFileNameByType("t2")), useOrigin=TRUE)
            
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=searchWidth)
            splines <- calculateSplinesForNeighbourhood(currentSession, neighbourhood, reference, faThreshold, nSamples, tracker=tracker)
            data <- createDataTableForSplines(splines, reference$getTract(), "knot", subjectId=i, neighbourhood=neighbourhood)
            write.table(data, file=sessionDatasetName)
        }
    })
    
    if (is.null(sessionNumbers))
    {
        allData <- NULL
        for (i in seq_along(sessionList))
        {
            sessionDatasetName <- ensureFileSuffix(paste(datasetName,"_session",i,sep=""), "txt")
            data <- read.table(sessionDatasetName)
            data$subject <- i

            allData <- rbind(allData, data)
        }

        write.table(allData, file=ensureFileSuffix(datasetName,"txt"))
        unlink(ensureFileSuffix(paste(datasetName,"_session",seq_along(sessionList),sep=""), "txt"))
    }
}
