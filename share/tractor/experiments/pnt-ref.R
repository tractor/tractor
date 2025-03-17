#@args session directory, seed point
#@desc Create a reference tract for probabilistic neighbourhood tractography. This requires a session and seed point. A maximum angle between spline knots (in degrees) can be specified to avoid retaining aberrant parts of the tract distal to the seed. Users should not change the LengthQuantile and RegisterCandidatesToReference options from their defaults unless they know what they are doing.
#@group Probabilistic neighbourhood tractography

library(tractor.track)
library(tractor.session)
library(tractor.nt)

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- attachMriSession(Arguments[1])
    seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    if (!exists("seed") || length(seed) != 3)
        report(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    nStreamlines <- getConfigVariable("Streamlines", 5000)
    maxAngle <- getConfigVariable("MaximumAngle", NULL, "numeric")
    tractName <- getConfigVariable("TractName", "tract")
    
    lengthQuantile <- getConfigVariable("LengthQuantile", 0.99)
    registerToReference <- getConfigVariable("RegisterCandidatesToReference", TRUE)
    
    if (!is.null(maxAngle))
        maxAngle <- maxAngle / 180 * pi
    
    options <- createTractOptionList("knot", lengthQuantile, registerToReference, NULL, NULL)
    returnValue <- referenceSplineTractWithOptions(options, session, seed, nStreamlines=nStreamlines, maxAngle=maxAngle)
    
    reference <- newReferenceTractWithTract(returnValue$spline, nativeSeed=seed, session=session, options=returnValue$options)
    writeNTResource(reference, "reference", list(tractName=tractName))
    
    invisible (returnValue$options)
}
