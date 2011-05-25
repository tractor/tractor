.onLoad <- function (libname, pkgname)
{
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (imageFileExists(file.path(tractorHome, "share", "mni", "brain")))
        .StandardBrainPath <<- file.path(tractorHome, "share", "mni")
}
