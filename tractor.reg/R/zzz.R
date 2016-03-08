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
    
    # Old transformations are not reliably distinguishable from current ones, so we retrofit an explicit version indicator
    registerDeserialiser("Transformation", function (fields) {
        if (!("version" %in% names(fields)))
            fields$version <- 1L
        return (do.call(Transformation$new, fields))
    })
}
