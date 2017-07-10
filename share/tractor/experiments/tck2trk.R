#@args tck file, reference image
#@desc Convert an MRtrix .tck file to TrackVis .trk format. The second (image) argument is used to provide the reference space for the tracks.

library(tractor.track)

runExperiment <- function ()
{
    tckStem <- ensureFileSuffix(Arguments[1], NULL, strip="tck")
    imageStem <- identifyImageFileNames(Arguments[2])$fileStem
    .Call("tck2trk", tckStem, imageStem, PACKAGE="tractor.track")
}
