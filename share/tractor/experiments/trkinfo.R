#@desc Print basic information about a streamline file in .trk format, including the number of streamlines stored and associated properties.
#@args streamline file
#@group Tractography
#@nohistory TRUE

library(tractor.track)

runExperiment <- function ()
{
    requireArguments("streamline file")
    
    streamSource <- readStreamlines(implode(Arguments, sep=" "))
    summary <- streamSource$summarise()
    printLabelledValues(names(summary), as.character(summary))
}
