#@args [session directory]
#@desc Print the status of a session directory (default ".") with regard to preprocessing for FSL and Camino. Preprocessing stages relate to those used by the "preproc" script.

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    if (getOption("outputLevel") > OL$Info)
        setOutputLevel(OL$Info)
    options(useOutputPrefix=FALSE)
    
    stagesComplete <- c(imageFileExists(session$getImageFileNameByType("rawdata","diffusion")),
                        imageFileExists(session$getImageFileNameByType("refb0","diffusion")),
                        imageFileExists(session$getImageFileNameByType("mask","diffusion")),
                        imageFileExists(session$getImageFileNameByType("fa","diffusion")))
    
    report(OL$Info, "Session directory:             ", session$getDirectory())
    report(OL$Info, "Working directory exists:      ", file.exists(session$getDirectory("root")))
    report(OL$Info, "Preprocessing stages complete: ", implode(which(stagesComplete), ","))
    report(OL$Info, "Number of fibres per voxel:    ", session$nFibres())
    report(OL$Info, "Camino files created:          ", file.exists(file.path(session$getDirectory("camino"), "sequence.scheme")))
}
