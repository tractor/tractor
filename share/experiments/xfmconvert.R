#@desc Convert old MNI-to-T2 transforms to the new format for TractoR 1.0 and above

library(tractor.base)
library(tractor.session)

runExperiment <- function ()
{
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)
    
    for (dir in sessionList)
    {
        session <- newSessionFromDirectory(dir)
        oldFileName <- session$getObjectFileName("xfmMniToT2")
        
        if (file.exists(oldFileName))
        {
            output(OL$Info, "Creating new transform file for session directory ", dir)
            xfm <- get(load(oldFileName))
            serialiseListObject(xfm, file=session$getObjectFileName("transformFromMni"))
        }
        else
            output(OL$Info, "No existing transform file found for session directory ", dir)
    }
}
