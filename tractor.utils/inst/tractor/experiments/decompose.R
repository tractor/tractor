#@args session directory, volume index

library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "volume index")
    
    session <- attachMriSession(Arguments[1])
    index <- as.numeric(Arguments[2])
    
    registration <- getVolumeTransformationForSession(session, "diffusion")
    decomposition <- RNiftyReg::decomposeAffine(registration$getTransforms(index, preferAffine=TRUE))
    
    
    report(OL$Info, "Translation (mm): ", implode(round(decomposition$translation,6),", "))
    report(OL$Info, "Scales          : ", implode(round(decomposition$scales,6),", "))
    report(OL$Info, "Skews           : ", implode(round(decomposition$skews,6),", "))
    report(OL$Info, "Rotations (rad) : ", implode(round(decomposition$angles,6),", "))
    
    invisible(NULL)
}
