suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    refTractFile <- getWithDefault("ReferenceTractFile", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    seedList <- getWithDefault("SeedPointList", NULL, "integer")
    pointType <- getWithDefault("PointType", NULL, mode="character")
    searchWidth <- getWithDefault("SearchWidth", 1)
    datasetName <- getWithDefault("DatasetName", "data")
    
    if (!all(c("seed","spline","options") %in% load(refTractFile)))
        output(OL$Error, "The file specified does not seem to contain reference tract information")
    if (!exists("refSession"))
        refSession <- NULL
    
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
        currentSession <- newSessionFromDirectory(sessionList[i])
        
        if (exists("seedMatrix"))
            currentSeed <- seedMatrix[i,]
        else
            currentSeed <- getNativeSpacePointForSession(currentSession, seed, pointType="r", isStandard=TRUE)
        
        if (pointType == "mm")
            currentSeed <- transformWorldToRVoxel(currentSeed, newMriImageMetadataFromFile(currentSession$getImageFileNameByType("t2")), useOrigin=TRUE)
        
        splines <- calculateSplinesForNeighbourhood(currentSession, currentSeed, refSession, options, searchWidth)
        data <- createDataTableForSplines(splines, spline, "knot", subjectId=i)
        
        if (is.null(allData))
            allData <- data
        else
            allData <- rbind(allData, data)
    }
    
    write.table(allData, file=ensureFileSuffix(datasetName,"txt"))
}
