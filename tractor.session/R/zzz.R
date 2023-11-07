.onLoad <- function (libname, pkgname)
{
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (imageFileExists(file.path(tractorHome, "share", "tractor", "mni", "brain")))
        .StandardBrainPath <<- file.path(tractorHome, "share", "tractor", "mni")
    
    if (is.null(getOption("tractorViewer")))
    {
        viewer <- tolower(Sys.getenv("TRACTOR_VIEWER"))
        if (isTRUE(viewer %in% .Viewers))
            options(tractorViewer=as.vector(viewer))
        else
            options(tractorViewer="tractor")
    }
    
    # Assume path separator (.Platform$file.sep) is "/"
    registerPathHandler("^([^@=\\s]+)?@(\\w+)(/)?([\\w.-/]+)?(%(\\d+))?$", function(path, index=1, defaultSessionPath=".") {
        groups <- groups(attr(path, "match"))
        groupsPresent <- !is.na(groups)
        
        # The string matches, so group 2 must be present
        session <- attachMriSession(ifelse(groupsPresent[1], groups[1], defaultSessionPath))
        if (groupsPresent[6])
            index <- as.integer(groups[6])
        if (groupsPresent[3] && groupsPresent[4])
            return (session$getImageFileNameByType(groups[4], groups[2], index=index, fallback=TRUE))
        else if (groupsPresent[3])
            return (session$getDirectory(groups[2]))
        else
            return (session$getImageFileNameByType(groups[2], index=index, fallback=TRUE))
    })
}
