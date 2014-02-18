#@args [session directory]
#@desc Runs Freesurfer's anatomical parcellation algorithms for the specified session (default "."). By default Freesurfer's "recon-all" script is run with just the "-all" option; additional program options may be specified using the Options configuration variable, if required. One or more T1-weighted images must be available in the session; these can be imported with the "import" script.

library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    force <- getConfigVariable("Force", FALSE)
    options <- getConfigVariable("Options", "-all", "character")
    parcellationName <- getConfigVariable("Parcellation", "desikan-killiany", validValues=c("desikan-killiany","destrieux","none"))
    
    if (!force && session$imageExists("desikan-killiany"))
        report(OL$Info, "Freesurfer has been previously run for this session - use Force:true to run it again")
    else
        runFreesurferForSession(session, options)
    
    if (!session$imageExists("reft1","structural") && session$imageExists("reft1","freesurfer"))
    {
        reft1 <- session$getImageByType("reft1", "freesurfer")
        writeImageFile(reft1, session$getImageFileNameByType("reft1","structural"))
    }
    
    if (parcellationName != "none" && !session$imageExists("parcellation","structural"))
    {
        regionFilePath <- file.path(Sys.getenv("TRACTOR_HOME"), "etc", "parcellations", ensureFileSuffix(parcellationName,"lut"))
        parcellation <- readParcellation(session$getImageFileNameByType(parcellationName), regionFilePath)
        parcellation$image <- transformImageToSpace(parcellation$image, session, "structural", "freesurfer", finalInterpolation=0)
        writeParcellation(parcellation, session$getImageFileNameByType("parcellation","structural"))
    }
}
