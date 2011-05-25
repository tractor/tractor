#@args session directory, volume index

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "volume index")
    
    session <- newSessionFromDirectory(Arguments[1])
    index <- as.numeric(Arguments[2])
    
    transform <- readEddyCorrectTransformsForSession(session, index)
    decomposition <- decomposeAffineTransform3D(transform[[1]])
    
    report(OL$Info, "Translation (mm): ", implode(round(decomposition$translation,6),", "))
    report(OL$Info, "Scales          : ", implode(round(decomposition$scales,6),", "))
    report(OL$Info, "Skews           : ", implode(round(decomposition$skews,6),", "))
    report(OL$Info, "Rotations (rad) : ", implode(round(decomposition$angles,6),", "))
    
    invisible(NULL)
}
