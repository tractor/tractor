#@args [session directory]
#@desc Runs Freesurfer's anatomical parcellation algorithms for the specified session (default "."). One or more T1-weighted images must be available in the session; these can be imported with the "sread" script.

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    force <- getConfigVariable("Force", FALSE)
    
    if (!force && imageFileExists(session$getImageFileNameByType("desikan_killiany")))
        report(OL$Info, "Freesurfer has been previously run for this session - use Force:true to run it again")
    else
        runFreesurferForSession(session)
}
