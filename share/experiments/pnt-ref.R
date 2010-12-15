#@args session directory, seed point
#@desc Create a reference tract for probabilistic neighbourhood tractography. This requires a session and seed point. A maximum angle between spline knots (in degrees) can be specified to avoid retaining aberrant parts of the tract distal to the seed. Users should not change the LengthQuantile and RegisterCandidatesToReference options from their defaults unless they know what they are doing.

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- newSessionFromDirectory(Arguments[1])
    seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    if (!exists("seed") || length(seed) != 3)
        output(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getWithDefault("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    tracker <- getWithDefault("Tracker", "fsl", validValues=c("fsl","tractor"))
    isStandardSeed <- getWithDefault("SeedInMNISpace", FALSE)
    nSamples <- getWithDefault("NumberOfSamples", 5000)
    maxAngle <- getWithDefault("MaximumAngle", NULL, "numeric")
    tractName <- getWithDefault("TractName", "tract")
    
    lengthQuantile <- getWithDefault("LengthQuantile", 0.99)
    registerToReference <- getWithDefault("RegisterCandidatesToReference", TRUE)
    
    if (!is.null(maxAngle))
        maxAngle <- maxAngle / 180 * pi
    
    seed <- getNativeSpacePointForSession(session, seed, pointType, isStandardSeed)
    
    options <- createTractOptionList("knot", lengthQuantile, registerToReference, NULL, NULL)
    returnValue <- referenceSplineTractWithOptions(options, session, seed, nSamples=nSamples, maxAngle=maxAngle, tracker=tracker)
    
    reference <- newReferenceTractWithTract(returnValue$spline, nativeSeed=seed, session=session, options=returnValue$options)
    writeNTResource(reference, "reference", "pnt", list(tractName=tractName))
    
    invisible (returnValue$options)
}
