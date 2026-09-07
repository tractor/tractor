#' @rdname getStandardImage
#' @export
getFileNameForStandardImage <- function (name = c("brain","face"), errorIfMissing = TRUE)
{
    if (is.null(.StandardBrainPath))
    {
        if (errorIfMissing)
            report(OL$Error, "Cannot find standard brain volumes")
        else
            return (NULL)
    }
    
    name <- match.arg(name)
    fileName <- switch(name, brain="brain", face="face_mask")
    
    return (file.path(.StandardBrainPath, fileName))
}

#' Obtain a standard-space (MNI) reference image
#'
#' These functions return the path to, or contents of, one of the standard
#' MNI-space reference images shipped with TractoR (under
#' `share/tractor/mni`), such as the standard brain volume used as a
#' registration target for the `"mni"` space (see [MriSession]).
#'
#' @param name For `getFileNameForStandardImage()`, one of `"brain"` or
#'   `"face"`. For `getStandardImage()`, any type name understood by
#'   `getFileNameForStandardImage()`.
#' @param errorIfMissing Boolean value: if `TRUE`, an error is raised if the
#'   standard images cannot be found; otherwise `NULL` is returned.
#' @param ... Additional arguments to [tractor.base::readImageFile()].
#' @return For `getFileNameForStandardImage()`, a string giving the path to
#'   the relevant image, or `NULL`. For `getStandardImage()`, the
#'   corresponding [tractor.base::MriImage] object, or `NULL`.
#' @author Jon Clayden
#' @export
getStandardImage <- function (name, errorIfMissing = TRUE, ...)
{
    invisible (readImageFile(getFileNameForStandardImage(name), ...))
}
