#@args [session directory]
#@desc Creates files required by the Camino diffusion toolkit under the specified session directory (or "." if none is given). A scheme file will be created, which describes the diffusion weighting applied to the data set, and the data images and brain mask will be written using the file formats that Camino recognises. All this information is copied from the FSL directory, so preprocessing with the "dpreproc" script must be completed before running this one.
#@interactive TRUE

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    createCaminoFilesForSession(session)
}
