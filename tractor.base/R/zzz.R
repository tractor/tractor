.onLoad <- function (libname, pkgname)
{
    if (is.null(getOption("tractorFileType")))
    {
        fileType <- toupper(Sys.getenv("TRACTOR_FILETYPE"))
        if (isTRUE(fileType %in% .FileTypes$typeNames))
            options(tractorFileType=as.vector(fileType))
        else
            options(tractorFileType="NIFTI_GZ")
    }
}
