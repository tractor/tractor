#' Copy a directory and its contents
#' 
#' This function copies a directory and its contents from one location to
#' another. Unlike \code{\link{file.copy}}, the target directory doesn't have
#' to exist, and the copied directory can have any name.
#' 
#' @param from A string giving the source directory.
#' @param to A string giving the target directory. If it doesn't exist it will
#'   be created and filled with the contents of the source directory. If it
#'   does exist, a new subdirectory with the same \code{\link{basename}} as the
#'   source directory will be created, and the files put there (as with
#'   \code{\link{file.copy}}, but see the \code{overwrite} parameter below).
#' @param allFiles If \code{TRUE}, hidden files (with names beginning with a
#'   period) will be included in the copy.
#' @param deleteOriginal If \code{TRUE}, the source directory will be deleted
#'   if the copy is successful.
#' @param overwrite If \code{TRUE}, an existing target directory will be
#'   removed and replaced with the contents of the source directory.
#' @return \code{TRUE} if all files were copied successfully; \code{FALSE}
#'   otherwise.
#' 
#' @author Jon Clayden
#' @seealso \code{\link{file.copy}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @export
copyDirectory <- function (from, to, allFiles = TRUE, deleteOriginal = FALSE, overwrite = FALSE)
{
    if (length(from) != 1 || length(to) != 1)
        report(OL$Error, "Source and target paths must be single strings")
    
    from <- expandFileName(from)
    to <- expandFileName(to)
    
    if (!file.exists(from))
        report(OL$Error, "The source directory does not exist")
    else if (!file.info(from)$isdir)
        report(OL$Error, "The source path does not point to a directory")
    
    if (file.exists(to))
    {
        if (overwrite)
        {
            if (unlink(to, recursive=TRUE) != 0)
                report(OL$Error, "Failed to delete the old directory")
        }
        else if (!file.info(to)$isdir)
            report(OL$Error, "Target path already exists but isn't a directory")
        else if (file.exists(file.path(to, basename(from))))
            report(OL$Error, "Subdirectory of the target directory matching the source name already exists - it will not be overwritten")
        else
            to <- file.path(to, basename(from))
    }
    
    # The "to" path should not exist at this point
    success <- dir.create(to, recursive=TRUE)
    
    files <- list.files(from, all.files=allFiles, full.names=FALSE, recursive=TRUE, include.dirs=TRUE)
    isDirectory <- file.info(file.path(from,files))$isdir
    
    for (dir in files[isDirectory])
    {
        if (!success)
            break
        success <- dir.create(file.path(to,dir), recursive=TRUE)
    }
    if (any(!isDirectory))
        success <- file.copy(file.path(from,files[!isDirectory]), file.path(to,files[!isDirectory]))
    
    if (deleteOriginal && all(success))
        unlink(from, recursive=TRUE)
    
    return (all(success))
}
