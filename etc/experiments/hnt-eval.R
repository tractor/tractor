suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    seedList <- getWithDefault("SeedPointList", NULL, "integer")
    pointType <- getWithDefault("PointType", NULL, mode="character")
    searchWidth <- getWithDefault("SearchWidth", 1)
    avfThreshold <- getWithDefault("AnisotropyThreshold", 0.2)
    resultsName <- getWithDefault("ResultsName", "results")
    
    refFileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    load(refFileName)
    if (!exists("reference") || !isReferenceTract(reference))
        output(OL$Error, "The file specified does not seem to contain reference tract information")
    if (!isFieldTract(reference))
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

    results <- list()
    for (i in seq_along(sessionList))
    {
        currentSession <- newSessionFromDirectory(sessionList[i])

        if (exists("seedMatrix"))
            currentSeed <- seedMatrix[i,]
        else
            currentSeed <- getNativeSpacePointForSession(currentSession, reference$getStandardSpaceSeedPoint(), pointType="r", isStandard=TRUE)

        if (pointType == "mm")
            currentSeed <- transformWorldToRVoxel(currentSeed, newMriImageMetadataFromFile(currentSession$getImageFileNameByType("t2")), useOrigin=TRUE)
        
        result <- runNeighbourhoodTractography(currentSession, currentSeed, reference$getTract(), avfThreshold, searchWidth)
        results <- c(results, list(result))
    }
    
    save(results, file=ensureFileSuffix(resultsName,"Rdata"))
}
