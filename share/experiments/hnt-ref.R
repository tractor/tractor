#@args session directory, seed point
#@desc Create a reference tract for use with heuristic neighbourhood tractography.
#@desc This is a matter of simply running the ProbTrack tractography algorithm with
#@desc an appropriate session and seed point, and the options are therefore closely
#@desc related to those of the "track" experiment.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- newSessionFromDirectory(Arguments[1])
    seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    if (!exists("seed") || length(seed) != 3)
        report(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    isStandardSeed <- getConfigVariable("SeedInMNISpace", FALSE)
    nSamples <- getConfigVariable("NumberOfSamples", 5000)
    tractName <- getConfigVariable("TractName", "tract")
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    tract <- newFieldTractFromProbtrack(session, seed, nSamples=nSamples, threshold=0.01)
    reference <- newReferenceTractWithTract(tract, session=session, nativeSeed=seed)
    
    writeNTResource(reference, "reference", "hnt", list(tractName=tractName))
}
