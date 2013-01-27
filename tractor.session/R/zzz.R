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
            options(tractorViewer="internal")
    }
}
