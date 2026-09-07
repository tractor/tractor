# Find the first external viewer whose executable is actually available
#' Find an available external image viewer
#'
#' This function determines the first available external image viewer
#' program, in order of preference, favouring the viewer named by the
#' `tractorViewer` option (see [showImagesInViewer()]) if its executable can
#' be located.
#'
#' @return A string naming the first available external viewer (one of
#'   `"fsleyes"`, `"fslview"`, `"freeview"` or `"mrview"`), or `NULL` if none
#'   can be found.
#' @author Jon Clayden
#' @export
externalViewer <- function ()
{
    binaries <- list(fsleyes="fsleyes", fslview=c("fslview","fslview_deprecated"), freeview="freeview", mrview="mrview")
    
    # Respect the tractorViewer option, but ignore the internal viewer
    viewers <- setdiff(c(getOption("tractorViewer"), .Viewers), "tractor")
    for (viewer in viewers)
    {
        for (binary in binaries[[viewer]])
        {
            if (!is.null(locateExecutable(binary, errorIfMissing=FALSE)))
                return (viewer)
        }
    }
}

#' Display one or more images
#'
#' This function displays one or more images using TractoR's own internal
#' viewer, or an external program, according to the specified or default
#' `viewer`. It handles conversion between colour lookup table/colour map
#' conventions used by the different viewers, region-of-interest lookup
#' tables associated with labelled images, and (for the internal viewer)
#' opacity blending.
#'
#' @param ... One or more images to display, each as an
#'   [tractor.base::MriImage] object or a string giving the path to an image
#'   file.
#' @param viewer A string naming the viewer to use: one of `"tractor"` (the
#'   internal viewer), `"fsleyes"`, `"fslview"`, `"freeview"` or `"mrview"`.
#'   The default is determined by the `tractorViewer` option, which in turn
#'   considers the `TRACTOR_VIEWER` environment variable.
#' @param interactive Boolean value: for the internal viewer, should the
#'   session be interactive (allowing the user to scroll through slices,
#'   etc.)?
#' @param wait Boolean value: for external viewers, should the function wait
#'   for the viewer program to exit before returning?
#' @param lookupTable An optional colour lookup table (or colour map) name,
#'   or vector/list of names, one per image, in a form recognised by the
#'   chosen viewer, or a generic name such as `"greyscale"` or `"heat"` that
#'   will be translated appropriately.
#' @param opacity An optional numeric vector of opacity values, in the range
#'   0 to 1, one per image.
#' @param infoPanel For the internal viewer, a function used to generate the
#'   information panel text. If `NULL`, a suitable default is chosen based on
#'   whether or not any of the images have an associated region lookup table.
#' @return This function is called for its side effect.
#' @seealso [externalViewer()], for choosing an available external viewer,
#'   and [showImagesInFreeview()] and related functions, which implement the
#'   external viewer backends.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
showImagesInViewer <- function (..., viewer = getOption("tractorViewer"), interactive = TRUE, wait = FALSE, lookupTable = NULL, opacity = NULL, infoPanel = NULL)
{
    viewer <- match.arg(viewer, .Viewers)
    imageList <- list(...)
    indexNames <- rep(list(NULL), length(imageList))
    
    if (is.null(lookupTable))
        lookupTable <- rep(list("greyscale"), length(imageList))
    else if (is.character(lookupTable))
        lookupTable <- rep(as.list(lookupTable), length.out=length(imageList))
    else if (!is.list(lookupTable))
        lookupTable <- rep(list(lookupTable), length(imageList))
    else
        lookupTable <- rep(lookupTable, length.out=length(imageList))
    
    if (!is.null(opacity) && any(opacity < 0 | opacity > 1))
        report(OL$Error, "Opacity values must be between 0 and 1")
    
    # Unmatched lookup table strings are passed through the function at the end of each list
    capitalise <- function (str) paste(toupper(substr(str,1,1)), tolower(substr(str,2,max(nchar(str)))), sep="")
    lookupTableMappings <- list(tractor=list(Greyscale=1,grayscale=1,greyscale=1,"Red-Yellow"=2,heat=2,.default=tolower),
                                fsleyes=list(grayscale="greyscale",greyscale="greyscale",heat="red-yellow",.default=tolower),
                                fslview=list(grayscale="Greyscale",greyscale="Greyscale",heat="Red-Yellow",.default=capitalise),
                                freeview=list(Greyscale="grayscale",greyscale="grayscale","Red-Yellow"="heat",.default=tolower),
                                mrview=list(grayscale="Gray",greyscale="Gray",heat="Hot","red-yellow"="Hot",.default=capitalise))
    lookupTable <- lapply(lookupTable, function (l) {
        if (is.character(l) && l %in% names(lookupTableMappings[[viewer]]))
            return (lookupTableMappings[[viewer]][[l]])
        else if (is.character(l))
            return (lookupTableMappings[[viewer]]$.default(l))
        else
            return (l)
    })
    
    if (viewer == "tractor")
    {
        images <- lapply(imageList, function (image) {
            if (is.character(image))
                return (readImageFile(image))
            else if (is(image, "MriImage"))
                return (image)
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        })
        
        colourScales <- lapply(lookupTable, tractor.base::getColourScale)
        for (i in seq_along(images))
        {
            if (!images[[i]]$isInternal() && file.exists(ensureFileSuffix(images[[i]]$getSource(),"lut")))
            {
                regions <- read.table(ensureFileSuffix(images[[i]]$getSource(),"lut"), header=TRUE, stringsAsFactors=FALSE)
                imageRange <- range(images[[i]], na.rm=TRUE)
                regions <- subset(regions, index>=imageRange[1] & index<=imageRange[2])
                colours <- rep(NA, imageRange[2]-imageRange[1]+1)
                colours[regions$index - imageRange[1] + 1] <- regions$colour
                colourScales[[i]] <- list(background="#000000", colours=colours)
                indexNames[[i]] <- structure(regions$label, names=as.character(regions$index))
            }
        }
        
        if (!is.null(opacity))
        {
            for (i in seq_along(images))
                colourScales[[i]]$colours <- shades::opacity(colourScales[[i]]$colours, shades::recycle(opacity))
        }
        
        if (is.null(infoPanel))
            infoPanel <- where(all(sapply(indexNames, is.null)), RNifti::defaultInfoPanel) %||% augmentedInfoPanel(indexNames)
        
        do.call(viewImages, list(images, interactive=interactive, colourScales=colourScales, infoPanel=infoPanel))
    }
    else
    {
        tempDir <- threadSafeTempFile()
        if (!file.exists(tempDir))
            dir.create(tempDir)
    
        imageFileNames <- lapply(seq_along(imageList), function (i) {
            if (is.character(imageList[[i]]))
            {
                imageInfo <- identifyImageFileNames(imageList[[i]], errorIfMissing=FALSE)
                if (is.null(imageInfo))
                {
                    report(OL$Warning, "Image file \"", imageList[[i]], "\" does not exist")
                    return(NULL)
                }
                
                if (viewer == "fslview" || viewer == "fsleyes")
                {
                    # fslview is fussy about data types, so read and write the image if necessary (writeImageFile() prefers ANALYZE-compatible datatypes)
                    if (imageInfo$format == "Mgh" || (imageInfo$format == "Nifti" && RNifti::niftiHeader(imageInfo$imageFile)$datatype > 64))
                    {
                        dir.create(file.path(tempDir, i))
                        imageLoc <- file.path(tempDir, i, basename(imageInfo$fileStem))
                        imageInfo <- writeImageFile(readImageFile(imageInfo$imageFile), imageLoc, fileType="NIFTI_GZ")
                    }
                }
            }
            else if (is(imageList[[i]], "MriImage"))
            {
                dir.create(file.path(tempDir, i))
                imageLoc <- file.path(tempDir, i, basename(imageList[[i]]$getSource()))
                imageInfo <- writeImageFile(imageList[[i]], imageLoc, fileType="NIFTI_GZ")
            }
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        
            return (imageInfo$imageFile)
        })
        
        if (viewer == "fsleyes")
            showImagesInFsleyes(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "fslview")
            showImagesInFslview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "freeview")
            showImagesInFreeview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "mrview")
            showImagesInMrview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        
        # If we're not waiting for the program to exit, we can't delete the images yet
        if (wait)
            unlink(tempDir, recursive=TRUE)
    }
}
