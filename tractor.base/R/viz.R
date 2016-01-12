#' @rdname colourScales
#' @export
interpolatePalette <- function (colours, n, ...)
{
    rampFunction <- colorRamp(colours, ...)
    colourMatrix <- round(rampFunction(0:(n-1)/(n-1)))
    rgbStrings <- apply(colourMatrix, 1, function (x) sprintf("#%02X%02X%02X",x[1],x[2],x[3]))
    return (rgbStrings)
}

#' Functions for working with colour scales or palettes
#' 
#' The \code{getColourScale} function can be used to obtain a standard or
#' customised colour scale for use in the package's image visualisation
#' functions. A graded palette of colours between two or more key colours can
#' be obtained using \code{interpolatePalette}.
#' 
#' Colour scales can be specified in any of three ways. Firstly, by a single
#' number, representing a predefined colour scale. Currently valid values are 1
#' (greyscale, black background), 2 (red to yellow heat scale, red background),
#' 3 (blue to red rainbow scale, blue background), 4 (blue to white to red
#' diverging scale, white background), 5 (white to red, white background) and 6
#' (white to blue, white background). Secondly, a single colour name can be
#' given (see \code{\link{colours}}); in this case the background will be
#' black. This is useful for binary images. Thirdly and most flexibly, a list
#' with two named elements can be given: \code{colours}, a vector of colours
#' representing the colour scale, perhaps created using \code{\link{rgb}}; and
#' \code{background}, a single colour representing the background.
#' 
#' @aliases getColourScale interpolatePalette
#' @param n For \code{getColourScale}, a number, colour name or list (see
#'   Details). For \code{interpolatePalette}, a single integer specifying the
#'   length of the interpolated palette.
#' @param colours A vector of colours to interpolate between, using any format
#'   recognised by \code{\link{colours}}.
#' @param \dots Additional arguments to \code{\link{colorRamp}}.
#' @return For \code{getColourScale}, a list with elements
#'   \describe{
#'     \item{colours}{A character-mode vector representing the colours in the
#'       scale, usually of length 100. This can be passed as a colour scale to
#'       R's plotting functions.}
#'     \item{background}{A single character string representing the background
#'       colour.}
#'   }
#' The \code{interpolatePalette} function returns a character-mode vector
#' representing the colours in the interpolated scale.
#' @author Jon Clayden
#' @seealso \code{\link{colours}}, \code{\link{rgb}}, \code{\link{colorRamp}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @examples
#' 
#' getColourScale(1)
#' 
#' interpolatePalette(c("red","yellow"), 10)
#' 
#' @rdname colourScales
#' @export
getColourScale <- function (n)
{
    if (is.list(n))
        return (n)
    else if (is.character(n))
        return (list(colours=c("black",n,n), background="black"))
    else
    {
        colours <- list(gray(0:99/99),
                        heat.colors(100),
                        rainbow(100, start=0.7, end=0.1),
                        interpolatePalette(c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"), 100),  # ColorBrewer "RdBu" diverging palette
                        interpolatePalette(c("#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"), 100),                                                    # Just the red part of "RdBu"
                        interpolatePalette(c("#F7F7F7","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061"), 100),                                                    # Just the blue part of "RdBu"
                        interpolatePalette(c("#800026","#BD0026","#E31A1C","#FC4E2A","#FD8D3C","#FEB24C","#FED976","#FFEDA0","#FFFFCC"), 100))  # ColorBrewer "YlOrRd" sequential palette
    
        if (n < 0)
            scale <- list(colours=rev(colours[[-n]]))
        else
            scale <- list(colours=colours[[n]])
        
        scale$background <- scale$colours[1]
        return (scale)
    }
}

maximumIntensityProjection <- function (image, axis)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    nDims <- image$getDimensionality()
    if (!(axis %in% 1:nDims))
        report(OL$Error, "Specified axis is not relevant for this image")
    
    planeAxes <- setdiff(1:nDims, axis)
    result <- apply(image$getData(), planeAxes, max)
    
    invisible(result)
}

#' Visualise MriImage objects
#' 
#' Visualise \code{MriImage} objects noninteractively using an R graphics
#' device. See \code{\link{viewImages}} for an interactive alternative. These
#' functions create 2D visualisations of 3D images by slicing or maximum
#' intensity projection.
#' 
#' @param images A list of \code{\link{MriImage}} objects.
#' @param image A single \code{\link{MriImage}} object.
#' @param modes A character vector of the same length as \code{images}, each
#'   element being \code{"slice"} or \code{"projection"} (or abbreviations),
#'   indicating which type of visualisation should be applied to each image.
#' @param colourScale,colourScales A single colour scale definition, or a list
#'   in the plural case. See \code{\link{getColourScale}}.
#' @param axis,axes A vector of axes along which slice/projection images should
#'   be created. 1 is left-right, 2 is anterior-posterior, 3 is
#'   superior-inferior.
#' @param x,y,z Integer vectors, each of length 1. Exactly one of these must be
#'   specified to indicate the plane of interest.
#' @param sliceLoc Like \code{x}, \code{y} and \code{z}, except that a point in
#'   3 dimensions is specified. Must not be \code{NA} for each \code{axis}
#'   requested.
#' @param device Either \code{"internal"} for display on the default graphics
#'   device, or \code{"png"} for creating PNG format image file(s).
#'   Abbreviations are fine.
#' @param alphaImages A list of \code{\link{MriImage}} objects to be used as
#'   transparency masks. Must be the same length as \code{images} if not
#'   \code{NULL}. \code{NULL} values in the list indicate no mask.
#' @param prefix,file A file name or prefix (to which \code{"axial"},
#'   \code{"coronal"} or \code{"sagittal"} will be added) to be used when
#'   \code{device} is \code{"png"}.
#' @param zoomFactor Factor by which to enlarge the image. Applies only when
#'   \code{device} is \code{"png"}.
#' @param filter Interpolation filter to be used by ImageMagick.
#' @param windowLimits Numeric vector of length 2 giving the limits of the
#'   colour scale, or \code{NULL} for limits matching the range of the image
#'   data. Passed as the \code{zlim} argument to \code{\link{image}}.
#' @param clearance Number of voxels' clearance to leave around each slice
#'   image in the contact sheet. Passed to \code{\link{newMriImageByTrimming}}.
#' @param nColumns Number of slices per row in the contact sheet grid. If
#'   \code{NULL}, the function will aim for a square grid.
#' @param add Overlay the graphic on a previous one. Used only when
#'   \code{device} is \code{"internal"}.
#' @return These functions are called for their side effects.
#' 
#' @note When the \code{device} option is set to \code{"png"}, ImageMagick is
#' required by these functions.
#' @author Jon Clayden
#' @seealso See \code{\link{viewImages}} for an interactive alternative, and
#' \code{\link{getColourScale}} for details of how colour scales are specified.
#' Also \code{\link{image}}, which is used as the underlying plot function.
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @aliases visualisation
#' @rdname visualisation
#' @export
createSliceGraphic <- function (image, x = NA, y = NA, z = NA, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    device <- match.arg(device)
    
    if (image$getDimensionality() == 2)
    {
        axisRelevance <- c(FALSE, FALSE)
        slice <- image$getData()
    }
    else if (image$getDimensionality() == 3)
    {
        dims <- image$getDimensions()
        axisShortNames <- c("x", "y", "z")
        axisRelevance <- !is.na(c(x, y, z))
        planeLoc <- c(x, y, z)[axisRelevance]

        if (length(which(axisRelevance)) != 1)
            report(OL$Error, "Exactly one of x, y and z must be specified")
        if (planeLoc < 1 || planeLoc > dims[axisRelevance])
            report(OL$Error, "Specified plane (", axisShortNames[axisRelevance], " = ", planeLoc, ") is out of bounds")

        slice <- extractDataFromMriImage(image, which(axisRelevance), planeLoc)
    }
    else
        report(OL$Error, "The \"createSliceGraphic\" function only handles 2D and 3D images")
    
    if (device == "internal")
    {
        fieldOfView <- image$getFieldOfView()[!axisRelevance]
        displayGraphic(slice, colourScale, add=add, windowLimits=windowLimits, asp=fieldOfView[2]/fieldOfView[1])
    }
    else if (device == "png")
    {
        tempFile <- threadSafeTempFile()
        pngDims <- round(abs(dims[!axisRelevance] * image$getVoxelDimensions()[!axisRelevance] * zoomFactor))
        writePng(slice, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

#' @rdname visualisation
#' @export
createProjectionGraphic <- function (image, axis, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    
    device <- match.arg(device)
    projection <- maximumIntensityProjection(image, axis)
    imageAxes <- !(1:3 %in% axis)
    
    if (device == "internal")
    {
        fieldOfView <- image$getFieldOfView()[imageAxes]
        displayGraphic(projection, colourScale, add=add, windowLimits=windowLimits, asp=fieldOfView[2]/fieldOfView[1])
    }
    else if (device == "png")
    {
        tempFile <- threadSafeTempFile()
        pngDims <- round(abs(image$getDimensions()[imageAxes] * image$getVoxelDimensions()[imageAxes] * zoomFactor))
        writePng(projection, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

#' @rdname visualisation
#' @export
createContactSheetGraphic <- function (image, axis, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() != 3)
        report(OL$Error, "The \"createContactSheetGraphic\" function only handles 3D images")
    
    device <- match.arg(device)
    
    if (!is.null(clearance))
    {
        originalDims <- image$getDimensions()
        if (length(clearance) == 1)
        {
            clearance <- rep(clearance, image$getDimensionality())
            clearance[axis] <- 0
        }
        image <- newMriImageByTrimming(image, clearance)
        padding <- pmax(0, clearance - (originalDims - image$getDimensions()))
    }
    else
        padding <- rep(0, image$getDimensionality())
    
    dims <- image$getDimensions()
    if (is.null(nColumns))
        nColumns <- ceiling(sqrt(dims[axis]))
    nRows <- ceiling(dims[axis] / nColumns)
    imageAxes <- axis != 1:3
    padding <- padding[imageAxes]
    
    data <- matrix(NA, nrow=nColumns*(dims[imageAxes][1]+2*padding[1]), ncol=nRows*(dims[imageAxes][2]+2*padding[2]))
    for (i in seq_len(dims[axis]))
    {
        chunkRow <- (i-1) %/% nColumns + 1
        chunkCol <- (i-1) %% nColumns + 1
        rows <- ((chunkCol-1):chunkCol) * dims[imageAxes][1] + 1:0 + (2*chunkCol-1)*padding[1]
        cols <- ((chunkRow-1):chunkRow) * dims[imageAxes][2] + 1:0 + (2*chunkRow-1)*padding[2]
        data[rows[1]:rows[2],cols[1]:cols[2]] <- extractDataFromMriImage(image, axis, i)
    }
    
    if (device == "internal")
    {
        fieldOfView <- abs(dim(data) * image$getVoxelDimensions()[imageAxes])
        displayGraphic(data, colourScale, add=add, windowLimits=windowLimits, asp=fieldOfView[2]/fieldOfView[1])
    }
    else if (device == "png")
    {
        tempFile <- threadSafeTempFile()
        pngDims <- round(abs(dim(data) * image$getVoxelDimensions()[imageAxes] * zoomFactor))
        writePng(data, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

#' @rdname visualisation
#' @export
createCombinedGraphics <- function (images, modes, colourScales, axes = 1:3, sliceLoc = NULL, device = c("internal","png"), alphaImages = NULL, prefix = "image", zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
{
    if (!is.list(images) || !is.list(colourScales))
        report(OL$Error, "Images and colour scales must be given as lists")
    if (!is.null(alphaImages) && !is.list(alphaImages))
        report(OL$Error, "Alpha images must be specified in a list")
    if (!is.null(windowLimits) && !is.list(windowLimits))
        report(OL$Error, "Window limits must be specified in a list")
    if (!is.numeric(axes) || any(axes < 1 | axes > 3))
        report(OL$Error, "Projection axes must be specified as a combination of 1 (x), 2 (y) or 3 (z)")
    
    modes <- match.arg(modes, c("slice","projection","contact"), several.ok=TRUE)
    if (any(modes == "slice") && is.null(sliceLoc))
        report(OL$Error, "Slice location must be specified")
    if (any(modes == "contact") && !all(modes == "contact"))
        report(OL$Error, "Contact slice mode must be used for all graphics or none")
    
    device <- match.arg(device)
    
    nImages <- length(images)
    if (!all(c(length(modes),length(colourScales)) == nImages))
        report(OL$Error, "Lengths of 'images', 'modes' and 'colourScales' do not all match")
    if (!is.null(alphaImages) && length(alphaImages) != nImages)
        report(OL$Error, "Lengths of 'images' and 'alphaImages' do not match")
    if (!is.null(windowLimits) && length(windowLimits) != nImages)
        report(OL$Error, "Lengths of 'images' and 'windowLimits' do not match")
    
    if (device == "png")
    {
        projectionNames <- c("sagittal", "coronal", "axial")
        imageFiles <- threadSafeTempFile(rep(basename(prefix), 2*nImages))
        combinedFiles <- threadSafeTempFile(rep(basename(prefix), 2))
        
        for (axis in axes)
        {
            if (!is.null(sliceLoc))
            {
                currentSliceLoc <- sliceLoc
                currentSliceLoc[setdiff(1:3,axis)] <- NA
            }
            currentFile <- paste(prefix, projectionNames[axis], sep="_")
            
            for (i in seq_along(images))
            {
                if (modes[i] == "slice")
                {
                    createSliceGraphic(images[[i]], currentSliceLoc[1], currentSliceLoc[2], currentSliceLoc[3], device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]])
                    if (!is.null(alphaImages[[i]]))
                        createSliceGraphic(alphaImages[[i]], currentSliceLoc[1], currentSliceLoc[2], currentSliceLoc[3], device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter)
                }
                else if (modes[i] == "projection")
                {
                    createProjectionGraphic(images[[i]], axis, device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]])
                    if (!is.null(alphaImages[[i]]))
                        createProjectionGraphic(alphaImages[[i]], axis, device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter)
                }
                else
                {
                    createContactSheetGraphic(images[[i]], axis, device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]], clearance=clearance, nColumns=nColumns)
                    if (!is.null(alphaImages[[i]]))
                        createContactSheetGraphic(alphaImages[[i]], axis, device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter, clearance=clearance, nColumns=nColumns)
                }
                
                if (i == 1)
                    file.copy(ensureFileSuffix(imageFiles[1],"png"), ensureFileSuffix(combinedFiles[1],"png"), overwrite=TRUE)
                else
                {
                    file.copy(ensureFileSuffix(combinedFiles[1],"png"), ensureFileSuffix(combinedFiles[2],"png"), overwrite=TRUE)
                    if (is.null(alphaImages[[i]]))
                        superimposePng(combinedFiles[2], imageFiles[2*i-1], combinedFiles[1])
                    else
                        superimposePng(combinedFiles[2], imageFiles[2*i-1], combinedFiles[1], imageFiles[2*i])
                }
            }
            
            file.copy(ensureFileSuffix(combinedFiles[1],"png"), ensureFileSuffix(currentFile,"png"), overwrite=TRUE)
            
            unlink(imageFiles)
            unlink(combinedFiles)
        }
    }
    else
        report(OL$Warning, "The 'createCombinedGraphics' function only supports the \"png\" device for now")
}
