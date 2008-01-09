#@desc Create a matching model for probabilistic neighbourhood tractography from a set
#@desc of B-spline tracts using a list of seed points and scan sessions, plus a refer-
#@desc ence tract, which must already exist

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    refTractFile <- getWithDefault("ReferenceTractFile", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    seedList <- getWithDefault("SeedPointList", NULL, "integer", errorIfMissing=TRUE)
    seedMatrix <- matrix(seedList, ncol=3, byrow=TRUE)
    pointType <- getWithDefault("PointType", NULL, mode="character", errorIfMissing=TRUE)
    pointType <- match.arg(tolower(pointType), c("fsl","r","mm"))
    maxKnotCount <- getWithDefault("MaximumKnotCount", NULL, "integer")
    modelName <- getWithDefault("ModelName", "model")
    
    if (pointType == "fsl")
        seedMatrix <- transformFslVoxelToRVoxel(seedMatrix)
    
    if (!all(c("seed","spline","options") %in% load(refTractFile)))
        output(OL$Error, "The file specified does not seem to contain reference tract information")
    if (!exists("refSession"))
        refSession <- NULL
    
    trainingSplines <- list()
    for (i in seq_along(sessionList))
    {
        currentSession <- newSessionFromDirectory(sessionList[i])
        currentSeed <- seedMatrix[i,]
        if (pointType == "mm")
            currentSeed <- transformWorldToRVoxel(currentSeed, newMriImageMetadataFromFile(currentSession$getImageFileNameByType("t2")), useOrigin=TRUE)
        currentSpline <- splineTractWithOptions(options, currentSession, currentSeed, refSession)
        trainingSplines <- c(trainingSplines, list(currentSpline))
    }
    
    data <- createDataTableForSplines(trainingSplines, spline, "knot")    
    model <- newMatchingTractModelFromDataTable(data, spline, maxLength=maxKnotCount)
    save(model, file=modelName)
    model$summarise()
}
