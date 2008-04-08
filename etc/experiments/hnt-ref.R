#@args session directory, seed point
#@desc Create a reference tract for use with heuristic neighbourhood tractography.
#@desc This is a matter of simply running the ProbTrack tractography algorithm with
#@desc an appropriate session and seed point, and the options are therefore closely
#@desc related to those of the "track" experiment.

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
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    tractName <- getWithDefault("TractName", "tract")
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    tract <- newFieldTractFromProbtrack(session, seed, nSamples=nSamples, threshold=0.01)
    reference <- newReferenceTractWithTract(tract, session=session, nativeSeed=seed)
    
    fileName <- ensureFileSuffix(paste(tractName,"ref",sep="_"), "Rdata")
    serialiseListObject(reference, file=fileName)
}
