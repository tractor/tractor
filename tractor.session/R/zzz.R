.onLoad <- function (libname, pkgname)
{
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (imageFileExists(file.path(tractorHome, "share", "mni", "brain")))
        .StandardBrainPath <<- file.path(tractorHome, "share", "mni")
    
    if (is.null(getOption("tractorViewer")))
    {
        viewer <- tolower(Sys.getenv("TRACTOR_VIEWER"))
        if (isTRUE(viewer %in% .Viewers))
            options(tractorViewer=as.vector(viewer))
        else
            options(tractorViewer="tractor")
    }
    
    # Assume path separator (.Platform$file.sep) is "/"
    registerPathHandler("^(.+)@(\\w+)(/(\\w+))?", function(path) {
        # The match has to have been done just before calling this function (although this is not thread-safe)
        groups <- groups(ore.lastmatch())
        nGroups <- sum(!is.na(groups))
        if (nGroups < 2)
            return (NULL)
        else if (nGroups < 3)
            newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[2])
        else
            newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[4], groups[2])
    })
}
