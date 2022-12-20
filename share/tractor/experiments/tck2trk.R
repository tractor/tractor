#@args tck file, reference image
#@desc Convert an MRtrix .tck file to TrackVis .trk format. The second (image) argument is used to provide the reference space for the tracks.

library(tractor.track)

runExperiment <- function ()
{
    tckStem <- ensureFileSuffix(Arguments[1], NULL, strip="tck")
    refImage <- readImageFile(Arguments[2])
    readStreamlines(tckStem)$process(tckStem, requireStreamlines=TRUE, refImage=refImage)
}
