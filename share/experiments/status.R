#@args [session directory]
#@desc Print the status of a session directory (default ".") with regard to preprocessing for FSL and Camino. Preprocessing stages relate to those used by the "preproc" script.

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    if (getOption("tractorOutputLevel") > OL$Info)
        setOutputLevel(OL$Info)
    options(tractorUseOutputPrefix=FALSE)
    
    stagesComplete <- c(imageFileExists(file.path(session$getPreBedpostDirectory(), "basic")),
                        imageFileExists(file.path(session$getPreBedpostDirectory(), "nodif")),
                        imageFileExists(session$getImageFileNameByType("mask")),
                        imageFileExists(session$getImageFileNameByType("fa")),
                        session$isPreprocessed())
    
    output(OL$Info, "Session directory:             ", session$getBaseDirectory())
    output(OL$Info, "Working directory exists:      ", file.exists(session$getWorkingDirectory()))
    output(OL$Info, "Preprocessing stages complete: ", implode(which(stagesComplete), ","))
    output(OL$Info, "Multifibre preprocessing:      ", !session$usesOldBedpost())
    output(OL$Info, "Number of fibres per voxel:    ", session$nFibres())
    output(OL$Info, "Camino files created:          ", file.exists(file.path(session$getCaminoDirectory(), "sequence.scheme")))
}
