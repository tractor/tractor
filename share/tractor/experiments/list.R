#@desc List all available TractoR experiment scripts, arranged into categories.
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    patterns <- c("DICOM handling"="(^dicom)|(^age$)",
                  "Image processing"="^(binarise|components|morph|smooth|trim)$",
                  "General analysis"="(^apply$)|(^image)|(^extract$)|(^values$)|(^mean$)|(^mkroi$)|(^reshape$)",
                  "Visualisation"="^(slice|view)$",
                  "Registration"="^reg-",
                  "Working with sessions"="^(clone|import|status|transform)$",
                  "Diffusion processing"="dpreproc|(^dir)|(^grad)|(^trk)|bedpost|tensor|track|plotcorrections",
                  "Structural processing"="^(deface|parcellate|freesurf)$",
                  "Probabilistic neighbourhood tractography"="^pnt-",
                  "Graph and network analysis"="^graph")
    
    standardDir <- file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "experiments")
    files <- sort(list.files(standardDir, "\\.R$"))
    filesWithoutExtensions <- ensureFileSuffix(files, NULL, strip="R")
    cat("\n")
    for (category in names(patterns))
    {
        cat(es("#{category}:\n  "))
        match <- filesWithoutExtensions %~% patterns[category]
        cat(filesWithoutExtensions[match], sep=", ", fill=TRUE)
        filesWithoutExtensions <- filesWithoutExtensions[!match]
        cat("\n")
    }
    
    if (length(filesWithoutExtensions) > 0)
    {
        cat(es("Other scripts:\n  "))
        cat(filesWithoutExtensions, sep=", ", fill=TRUE)
        cat("\n")
    }
    
    pathDirs <- unlist(strsplit(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE))
    for (pathDir in pathDirs)
    {
        files <- list.files(pathDir, "\\.R$")
        filesWithoutExtensions <- ensureFileSuffix(files, NULL, strip="R")
        cat(es("Additional scripts found in #{pathDir}:\n  "))
        cat(filesWithoutExtensions, sep=", ", fill=TRUE)
        cat("\n")
    }
    
    cat("For information on a particular script, run \"tractor -o <script>\"\n\n", file=stderr())
}
