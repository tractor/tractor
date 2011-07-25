#@args [session directory]
#@desc Print the status of a session directory (default ".") with regard to preprocessing for FSL and Camino. Preprocessing stages relate to those used by the "preproc" script.

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    stagesComplete <- c(imageFileExists(session$getImageFileNameByType("rawdata","diffusion")),
                        imageFileExists(session$getImageFileNameByType("refb0","diffusion")),
                        imageFileExists(session$getImageFileNameByType("mask","diffusion")),
                        imageFileExists(session$getImageFileNameByType("fa","diffusion")))
    
    labels <- c("Session directory", "Working directory exists", "Preprocessing stages complete", "Number of fibres per voxel", "Camino files created")
    values <- c(session$getDirectory(), file.exists(session$getDirectory("root")), implode(which(stagesComplete),","), session$nFibres(), file.exists(file.path(session$getDirectory("camino"),"sequence.scheme")))
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    printLabelledValues(labels, values)
}
