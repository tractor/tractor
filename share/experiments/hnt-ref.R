#@args session directory, seed point
#@desc Create a reference tract for use with heuristic neighbourhood tractography. This is a matter of simply running tractography with an appropriate session and seed point.

library(tractor.reg)
library(tractor.session)
library(tractor.nt)
library(tractor.track)

runExperiment <- function ()
{
    requireArguments("session directory", "seed point")
    
    session <- newSessionFromDirectory(Arguments[1])
    seed <- splitAndConvertString(Arguments[-1], ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
    if (!exists("seed") || length(seed) != 3)
        report(OL$Error, "Seed point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getConfigVariable("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    isStandardSeed <- getConfigVariable("SeedInMNISpace", FALSE)
    nStreamlines <- getConfigVariable("Streamlines", 5000)
    tractName <- getConfigVariable("TractName", "tract")
    
    if (isStandardSeed)
        seed <- transformPointsToSpace(seed, session, "diffusion", oldSpace="mni", pointType=pointType, outputVoxel=TRUE, nearest=TRUE)
    else
        seed <- round(changePointType(seed, session$getRegistrationTarget("diffusion",metadataOnly=TRUE), "r", pointType))
    
    trackerPath <- session$getTracker()$run(seed, nStreamlines, requireMap=TRUE)
    image <- readImageFile(trackerPath)$threshold(0.01*nStreamlines)
    tract <- newFieldTractFromMriImage(image, seed)
    reference <- newReferenceTractWithTract(tract, session=session, nativeSeed=seed)
    
    writeNTResource(reference, "reference", "hnt", list(tractName=tractName))
}
