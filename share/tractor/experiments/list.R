#@desc List all available TractoR experiment scripts.
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    pathDirs <- file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "experiments")
    pathDirs <- c(pathDirs, unlist(strsplit(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE)))
    
    for (pathDir in pathDirs)
    {
        files <- list.files(pathDir, "\\.R$")
        filesWithoutExtensions <- ensureFileSuffix(files, NULL, strip="R")
        cat(paste("Experiment scripts found in ", pathDir, ":\n", sep=""))
        print(noquote(filesWithoutExtensions))
        cat("\n")
    }
}
