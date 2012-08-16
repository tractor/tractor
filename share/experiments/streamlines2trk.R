#@args streamlines file
#@desc Convert streamlines saved by "track" or "mtrack" to the .trk format used by the TrackVis program.

library(splines)
library(tractor.nt)

runExperiment <- function ()
{
    requireArguments("streamlines file")
    streamlines <- deserialiseReferenceObject(Arguments[1])
    
    if (!is(streamlines, "StreamlineCollectionTract"))
        report(OL$Error, "The specified file does not contain a StreamlineCollectionTract object")
    
    fileName <- ensureFileSuffix(Arguments[1], "trk", strip="Rdata")
    writeStreamlineCollectionTractToTrackvis(streamlines, fileName)
    
    invisible(NULL)
}
