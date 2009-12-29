#@args session directory, volume index

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "volume index")
    
    session <- newSessionFromDirectory(Arguments[1])
    index <- as.numeric(Arguments[2])
    
    transform <- readEddyCorrectTransformsForSession(session, index)
    decomposition <- decomposeAffineTransform3D(transform[[1]])
    
    output(OL$Info, "Translation (mm): ", implode(round(decomposition$translation,6),", "))
    output(OL$Info, "Scales          : ", implode(round(decomposition$scales,6),", "))
    output(OL$Info, "Skews           : ", implode(round(decomposition$skews,6),", "))
    output(OL$Info, "Rotations (rad) : ", implode(round(decomposition$angles,6),", "))
    
    invisible(NULL)
}
