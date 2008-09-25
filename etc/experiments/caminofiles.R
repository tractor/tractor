#@args [session directory]
#@desc Creates files required by the Camino diffusion toolkit under the specified
#@desc session directory (or "." if none is given). A scheme file will be created,
#@desc which describes the diffusion weighting applied to the data set, and the
#@desc data images and brain mask will be written using the file formats that
#@desc Camino recognises. All this information is copied from the FSL directory,
#@desc so preprocessing with the "preproc" script must be completed before running
#@desc this one. The script will prompt for the diffusion time (in seconds) of the
#@desc data set if it is not specified using the DiffusionTime option.

suppressPackageStartupMessages(require(tractor.fsl))
suppressPackageStartupMessages(require(tractor.camino))

runExperiment <- function ()
{
    if (nArguments() == 0)
        session <- newSessionFromDirectory(".")
    else
        session <- newSessionFromDirectory(Arguments[1])
    
    diffusionTime <- getWithDefault("DiffusionTime", NULL, "numeric")
    
    createCaminoFilesForSession(session, diffusionTime)
}
