.onLoad <- function (libname, pkgname)
{
    if (is.null(getOption("tractorRegistrationMethod")))
    {
        regMethod <- tolower(Sys.getenv("TRACTOR_REG_METHOD"))
        if (isTRUE(regMethod %in% c("niftyreg","fsl")))
            options(tractorRegistrationMethod=as.vector(regMethod))
        else
            options(tractorRegistrationMethod="fsl")
    }
}
