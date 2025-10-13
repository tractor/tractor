.onLoad <- function (libname, pkgname)
{
    if (is.null(getOption("tractorRegistrationMethod")))
    {
        regMethod <- tolower(Sys.getenv("TRACTOR_REG_METHOD"))
        if (isTRUE(regMethod %in% c("niftyreg","fsl")))
            options(tractorRegistrationMethod=as.vector(regMethod))
        else
            options(tractorRegistrationMethod="niftyreg")
    }
    
    # Lightly specialised deserialiser, to turn relative paths back into absolute ones
    registerDeserialiser("Registration", function (fields, file = NULL, ...) {
        if (is.character(fields$source) && !is.null(file))
            fields$source <- imageFiles(expandFileName(fields$source,dirname(file)))$stems()
        if (is.character(fields$target) && !is.null(file))
            fields$target <- imageFiles(expandFileName(fields$target,dirname(file)))$stems()
        
        object <- Registration$new(fields$source, fields$target, fields$method, n=fields$n, transforms=fields$transforms)
        return (object)
    })
}

#' @import methods reportr tractor.base RNiftyReg
NULL
