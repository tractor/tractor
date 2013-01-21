#@desc Create B-spline tract representations and calculate characteristics of interest for a set of seed points in one or more brain volumes. This is a prerequisite for training or using a tract matching model. For training, a specific list of seed points will likely be required, in which case PointType should also be set. For using a model, the SeedPointList need not be given, in which case a region of width SearchWidth voxels around the reference tract seed point will be used, subject to the specified AnisotropyThreshold. The TractName specified must match that given to the "pnt-ref" experiment. If the SessionNumbers option is used to select a subset of the data sets, manual collation using "pnt-collate" will be required.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getConfigVariable("SessionList", NULL, "character", errorIfMissing=TRUE)
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    seedPoint <- getConfigVariable("SeedPoint", NULL, "character")
    seedList <- getConfigVariable("SeedPointList", NULL, "integer")
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE)
    searchWidth <- getConfigVariable("SearchWidth", 1)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.2)
    nSamples <- getConfigVariable("NumberOfSamples", 1000)
    datasetName <- getConfigVariable("DatasetName", "data")
    sessionNumbers <- getConfigVariable("SessionNumbers", NULL, "character")
    resume <- getConfigVariable("Resume", FALSE)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    collateData <- TRUE
    
    if (!is.null(seedPoint) && !is.null(seedList))
        report(OL$Error, "Only one of \"SeedPoint\" and \"SeedPointList\" should be given")
    
    if (is.null(seedPoint) && is.null(seedList))
        pointType <- "r"
    else
    {
        if (is.null(pointType))
            report(OL$Error, "Point type must be specified with the seed point(s)")
        
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
    {
        sessionNumbers <- splitAndConvertString(sessionNumbers, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        collateData <- FALSE
    }
    
    parallelApply(sessionNumbers, function (i) {
        sessionDatasetName <- ensureFileSuffix(paste(datasetName,"_session",i,sep=""), "txt")
        
        if (resume && file.exists(sessionDatasetName))
            report(OL$Info, "Using existing output for session ", i)
        else
        {
            report(OL$Info, "Calculating splines for session ", i)
            
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
                currentSeed <- transformWorldToRVoxel(currentSeed, currentSession$getImageByType("maskedb0",metadataOnly=TRUE), useOrigin=TRUE)
            
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=searchWidth)
            splines <- calculateSplinesForNeighbourhood(currentSession, neighbourhood, reference, faThreshold, nSamples, tracker=tracker)
            data <- createDataTableForSplines(splines, reference$getTract(), "knot", subjectId=i, neighbourhood=neighbourhood)
            write.table(data, file=sessionDatasetName)
        }
    })
    
    if (collateData)
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
