#@args session directory, volume index

library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "volume index")
    
    session <- newSessionFromDirectory(Arguments[1])
    index <- as.numeric(Arguments[2])
    
    transform <- getVolumeTransformationForSession(session, "diffusion")
    decomposition <- decomposeTransformation(transform)[[index]]
    
    report(OL$Info, "Translation (mm): ", implode(round(decomposition$translation,6),", "))
    report(OL$Info, "Scales          : ", implode(round(decomposition$scales,6),", "))
    report(OL$Info, "Skews           : ", implode(round(decomposition$skews,6),", "))
    report(OL$Info, "Rotations (rad) : ", implode(round(decomposition$angles,6),", "))
    
    invisible(NULL)
}
