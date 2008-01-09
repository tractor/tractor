suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- newSessionFromDirectory(Arguments[1])
    seed <- as.numeric(unlist(strsplit(Arguments[-1], ",")))
    if (!exists("seed") || length(seed) != 3)
        output(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getWithDefault("PointType", NULL, mode="character", errorIfMissing=TRUE)
    isStandardSeed <- getWithDefault("SeedInMNISpace", FALSE)
    tractName <- getWithDefault("TractName", "ref")
    
    lengthQuantile <- getWithDefault("LengthQuantile", 0.99)
    registerToReference <- getWithDefault("RegisterCandidatesToReference", TRUE)
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    
    options <- createTractOptionList("knot", lengthQuantile, registerToReference, NULL, NULL)
    returnValue <- referenceSplineTractWithOptions(options, session, seed)
    
    xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
    standardSeed <- round(transformVoxelPointsWithAffine(xfm,seed), 2)
    
    returnValue <- c(returnValue, list(refSession=session, seed=standardSeed))
    with(returnValue, save(seed, spline, options, refSession, file=paste(tractName,"Rdata",sep=".")))
    
    return (returnValue$options)
}
