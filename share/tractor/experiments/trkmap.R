#@desc Create a visitation map from a set of streamlines. This is a spatial histogram, with voxel values indicating the number (or proportion, with Normalise:true) of streamlines that pass through. By default all voxels traversed by each streamline are included, but the scope of the map can be reduced to just seed points (if stored in the file) or streamline end-points.
#@args streamline file, reference image

library(tractor.track)

runExperiment <- function ()
{
    requireArguments("streamline file", "reference image")
    
    scope <- getConfigVariable("Scope", "full", validValues=c("full","seed","ends"))
    normalise <- getConfigVariable("Normalise", FALSE)
    
    streamSource <- StreamlineSource$new(Arguments[1])
    map <- streamSource$getVisitationMap(identifyImageFileNames(Arguments[2])$fileStem, scope, normalise)
    
    outputStem <- ensureFileSuffix(basename(Arguments[1]), NULL, strip="trk")
    if (scope != "full")
        outputStem <- paste(outputStem, scope, sep="_")
    writeImageFile(map, outputStem)
}
