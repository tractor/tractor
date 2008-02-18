#@args session directory, seed point

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
    tractName <- getWithDefault("TractName", "tract")
    
    lengthQuantile <- getWithDefault("LengthQuantile", 0.99)
    registerToReference <- getWithDefault("RegisterCandidatesToReference", TRUE)
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    
    options <- createTractOptionList("knot", lengthQuantile, registerToReference, NULL, NULL)
    returnValue <- referenceSplineTractWithOptions(options, session, seed)
    
    reference <- newReferenceTractWithTract(returnValue$spline, nativeSeed=seed, session=session, options=returnValue$options)
    
    fileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    save(reference, file=fileName)
    
    invisible (returnValue$options)
}
