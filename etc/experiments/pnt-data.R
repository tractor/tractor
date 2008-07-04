#@desc Create B-spline tract representations and calculate characteristics of interest
#@desc for a set of seed points in one or more brain volumes. This is a prerequisite
#@desc for training or using a tract matching model. For training, a specific list of
#@desc seed points will likely be required, in which case PointType should also be
#@desc set. For using a model, the SeedPointList need not be given, in which case a
#@desc region of width SearchWidth voxels around the reference tract seed point will
#@desc be used, subject to the specified AnisotropyThreshold. The TractName specified
#@desc must match that given to the "pnt-ref" experiment.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    seedList <- getWithDefault("SeedPointList", NULL, "integer")
    pointType <- getWithDefault("PointType", NULL, mode="character")
    searchWidth <- getWithDefault("SearchWidth", 1)
    faThreshold <- getWithDefault("AnisotropyThreshold", 0.2)
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    datasetName <- getWithDefault("DatasetName", "data")
    resume <- getWithDefault("Resume", FALSE)
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    reference <- deserialiseReferenceTract(refFileName)
    if (!isBSplineTract(reference))
        output(OL$Error, "The specified reference tract is not in the correct form")
    
    if (is.null(seedList))
        pointType <- "r"
    else
    {
        if (is.null(pointType))
            output(OL$Error, "Point type must be specified with the seed list")
        
        pointType <- match.arg(tolower(pointType), c("fsl","r","mm"))
        seedMatrix <- matrix(seedList, ncol=3, byrow=TRUE)
        
        if (pointType == "fsl")
            seedMatrix <- transformFslVoxelToRVoxel(seedMatrix)
    }
    
    allData <- NULL
    for (i in seq_along(sessionList))
    {
        sessionDatasetName <- ensureFileSuffix(paste(datasetName,"_session",i,sep=""), "txt")
        
        if (resume && file.exists(sessionDatasetName))
            data <- read.table(sessionDatasetName)
        else
        {
            currentSession <- newSessionFromDirectory(sessionList[i])
        
            if (exists("seedMatrix"))
                currentSeed <- seedMatrix[i,]
            else
                currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType="r", isStandard=TRUE)
        
            if (pointType == "mm")
                currentSeed <- transformWorldToRVoxel(currentSeed, newMriImageMetadataFromFile(currentSession$getImageFileNameByType("t2")), useOrigin=TRUE)
        
            splines <- calculateSplinesForNeighbourhood(currentSession, currentSeed, reference$getSourceSession(), reference$getTractOptions(), searchWidth, faThreshold, nSamples)
            data <- createDataTableForSplines(splines, reference$getTract(), "knot", subjectId=i)
            write.table(data, file=sessionDatasetName)
        }
        
        if (is.null(allData))
            allData <- data
        else
            allData <- rbind(allData, data)
    }
    
    write.table(allData, file=ensureFileSuffix(datasetName,"txt"))
    
    for (i in seq_along(sessionList))
        unlink(ensureFileSuffix(paste(datasetName,"_session",i,sep=""), "txt"))
}
