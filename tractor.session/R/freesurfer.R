#' Run FreeSurfer's recon-all pipeline for a session
#'
#' This function runs FreeSurfer's `recon-all` cortical reconstruction
#' pipeline for a session's structural image, via the `reconall` TractoR
#' workflow (see [runWorkflow()]). FreeSurfer must be installed and set up
#' appropriately for this function to work. Any existing FreeSurfer output
#' for the session is removed first.
#'
#' @param session An [MriSession] object.
#' @param options A string, or character vector, of additional command-line
#'   options to pass to `recon-all`.
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runFreesurferForSession <- function (session, options = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    session$unlinkDirectory("freesurfer")
    runWorkflow("reconall", session, FreesurferOptions=implode(options," "))
}

#' Display images in an external viewer program
#'
#' These functions display one or more images by launching an external image
#' viewer program as a subprocess: `showImagesInFreeview()` for FreeSurfer's
#' `freeview`, `showImagesInFsleyes()` for FSL's `fsleyes`,
#' `showImagesInFslview()` for FSL's (deprecated) `fslview`, and
#' `showImagesInMrview()` for MRtrix's `mrview`. The relevant program must be
#' installed and on the system's path. These functions are not usually called
#' directly by users; instead see [showImagesInViewer()], which also supports
#' TractoR's own internal image viewer and handles image format conversion
#' where necessary.
#'
#' @param imageFileNames A character vector of paths to image files to
#'   display. The first is taken to be the main image, and any others are
#'   shown as overlays on top of it.
#' @param wait Boolean value: should the function wait for the viewer program
#'   to exit before returning?
#' @param lookupTable An optional character vector of colour lookup table (or
#'   colour map) names, of the same length as `imageFileNames`, or which will
#'   be recycled to that length.
#' @param opacity An optional numeric vector of opacity values, in the range
#'   0 to 1, of the same length as `imageFileNames`, or which will be
#'   recycled to that length.
#' @return The image file names, invisibly.
#' @seealso [showImagesInViewer()], which provides a unified, higher-level
#'   interface to these and TractoR's internal viewer.
#' @author Jon Clayden
#' @export
showImagesInFreeview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    validColourMaps <- c("grayscale","lut","heat","jet","gecolor","nih")
    
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        valid <- lookupTable %in% validColourMaps
        if (any(!valid))
            report(OL$Warning, "Lookup table name(s) ", implode(unique(lookupTable[!valid]),sep=", ",finalSep=" and "), " are not valid for freeview")
        
        imageFileNames <- paste(imageFileNames, ":colormap=", lookupTable, sep="")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, ":opacity=", opacity, sep="")
    }
    
    execute("freeview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}
