#' @import methods ore reportr RNifti
#' @importFrom grDevices col2rgb colorRamp dev.cur dev.off gray heat.colors rainbow rgb
#' @importFrom graphics image layout lines locator par plot strwidth text axis polygon
#' @importFrom stats na.omit cor kmeans
#' @importFrom utils read.table
NULL

.onLoad <- function (libname, pkgname)
{
    if (is.null(getOption("tractorFileType")))
    {
        fileType <- toupper(Sys.getenv("TRACTOR_FILETYPE"))
        if (isTRUE(fileType %in% .FileTypes$typeNames) && fileType != "NIFTI_GZ")
        {
            report(OL$Warning, "The TRACTOR_FILETYPE environment variable is deprecated - NIfTI will always be the default in future")
            options(tractorFileType=as.vector(fileType))
        }
        else
            options(tractorFileType="NIFTI_GZ")
    }
    
    if (is.null(getOption("tractorOutputPrecision")))
    {
        outputPrecision <- tolower(Sys.getenv("TRACTOR_OUTPUT_PRECISION"))
        if (isTRUE(outputPrecision %in% c("single","double")))
            options(tractorOutputPrecision=as.vector(outputPrecision))
        else
            options(tractorOutputPrecision="double")
    }
    
    if (is.null(getOption("tractorNoSymlinks")) && tolower(Sys.getenv("TRACTOR_NOSYMLINKS")) %in% c("1","yes","true"))
        options(tractorNoSymlinks=TRUE)
}
