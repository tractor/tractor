#@args [session directory]
#@desc Rotate diffusion gradient directions for the given session directory (default "."), to compensate for the eddy current correction process which is run as stage 2 of the "preproc" script. This is particularly advisable if the subject has moved a lot during the scan. (The "plotcorrections" script can be used to see how large this effect will be.)

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    rotateGradientVectorsForSession(session)
}
