#' @rdname viewer
#' @export
augmentedInfoPanel <- function (indexNames = NULL)
{
    if (is.null(indexNames))
        return (RNifti::defaultInfoPanel)
    else
    {
        if (!is.list(indexNames))
            indexNames <- list(indexNames)
        return (function (point, data, labels) {
            data <- lapply(seq_along(data), function(i) {
                if (is.null(indexNames[[i]]))
                    data[[i]]
                else
                    es("#{data[[i]] (#{indexNames[[i]][as.character(data[[i]])]})}")
            })
            RNifti::defaultInfoPanel(point, data, labels)
        })
    }
}

#' @rdname viewer
#' @export
timeSeriesPanel <- function (point, data, labels)
{
    escapeToQuit <- isTRUE(names(dev.cur()) %in% c("quartz","RStudioGD"))
    quitInstructions <- paste(ifelse(escapeToQuit,"Press Esc","Right click"), "to leave interactive mode", sep=" ")
    
    lengths <- sapply(data, length)
    suppressWarnings(range <- c(min(sapply(data,min,na.rm=T)), max(sapply(data,max,na.rm=T))))
    range[is.infinite(range)] <- 0
    plot(NA, xlim=c(1,max(lengths)), ylim=range, xlab=quitInstructions, ylab="intensity", col.lab="grey70", bty="n", main=paste("Location: (",implode(point,","),")",sep=""))
    
    for (i in seq_along(data))
    {
        if (lengths[i] > 1)
            lines(1:lengths[i], data[[i]], col="red", lwd=2)
    }
}

#' @rdname viewer
#' @export
polarPlotPanel <- function (directions, bValues = NULL)
{
    escapeToQuit <- isTRUE(names(dev.cur()) %in% c("quartz","RStudioGD"))
    quitInstructions <- paste(ifelse(escapeToQuit,"Press Esc","Right click"), "to leave interactive mode", sep=" ")
    
    if (is.null(bValues))
        bValues <- rep(1, nrow(directions))
    
    return (function (point, data, labels) {
        lengths <- sapply(data, length)
        data <- data[[which(lengths==nrow(directions))]]
        maxDataValue <- suppressWarnings(max(data[bValues>0],na.rm=T))
    
        correlations <- abs(cor(cbind(abs(directions), data))[1:3,4])
        basicColour <- rgb(correlations[1], correlations[2], correlations[3])
        basicBrightness <- shades::brightness(basicColour)
        axes <- setdiff(1:3, which.min(correlations))
        view <- c("sagittal","coronal","axial")[which.min(correlations)]
    
        plot(NA, xlim=c(-maxDataValue,maxDataValue), ylim=c(-maxDataValue,maxDataValue), xlab=quitInstructions, ylab="intensity", col.lab="grey70", bty="n", main=paste("Location: (",implode(point,","),")\nView: ",view,sep=""), xaxt="n", yaxt="n", asp=1)
        ticks <- list(x=pretty(par("xaxp")[1:2]), y=pretty(par("yaxp")[1:2]))
        axis(1, ticks$x, abs(ticks$x))
        axis(2, ticks$y, abs(ticks$y))
    
        for (b in setdiff(sort(unique(bValues)),0))
        {
            i <- (bValues == b)
            currentDirections <- rbind(directions[i,], -directions[i,])
            currentData <- rep(data[i],2)
            order <- order(atan2(currentDirections[,axes[2]], currentDirections[,axes[1]]))
            polygon((currentDirections[,axes]*currentData)[order,], col=shades::brightness(basicColour,basicBrightness*b/max(bValues,na.rm=TRUE)))
        }
    })
}

#' A simple interactive viewer for MriImage objects
#' 
#' The \code{viewImages} function provides a simple interactive viewer for
#' \code{MriImage} objects. 3D and 4D images may be used.
#' 
#' @param images An \code{MriImage} object, or list of \code{MriImage} objects.
#' @param colourScales A list of colour scales to use for each image, which
#'   will be recycled to the length of \code{images}. See
#'   \code{\link{getColourScale}} for details. The default is to use greyscale.
#' @param point For \code{viewImages}, a length 3 integer vector giving the
#'   initial location of the crosshairs, in voxels. For info panel functions,
#'   the current location of the crosshairs.
#' @param interactive A single logical value. If \code{TRUE}, the plot is
#'   interactive.
#' @param crosshairs A single logical value. If \code{TRUE}, the crosshairs are
#'   displayed.
#' @param orientationLabels A single logical value. If \code{TRUE}, orientation
#'   labels are displayed.
#' @param fixedWindow A single logical value. If \code{TRUE}, each image is
#'   windowed globally, rather than for each slice.
#' @param indexNames A list whose elements are either \code{NULL} or a named
#'   character vector giving the names associated with each index in the image.
#' @param infoPanel A function with at least three arguments, which must plot
#'   something to fill the bottom-right panel of the viewer after each change
#'   of crosshair location. The three mandatory arguments correspond to the
#'   current location in the image, the image values at that location, and the
#'   names of each image. The \code{defaultInfoPanel} and
#'   \code{timeSeriesPanel} functions are valid examples.
#' @param \dots Additional arguments to \code{infoPanel}.
#' @param data A list giving the data value(s) at the current crosshair
#'   location in each image displayed. Typically numeric, but in principle may
#'   be of any mode, and will be character mode when \code{indexNames} is not
#'   \code{NULL}.
#' @param imageNames A character vector giving a name for each image displayed.
#' @param directions A matrix of 3D acquisition direction vectors, one per row.
#' @param bValues A vector of b-values, if the image is diffusion-weighted.
#' @return These functions are called for their side effects.
#' 
#' @note The \code{defaultInfoPanel} and \code{timeSeriesPanel} functions are
#'   not intended to be called directly. They are simple examples of valid
#'   values for the \code{infoPanel} argument to \code{viewImages}.
#' @author Jon Clayden
#' @seealso \code{\link{getColourScale}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @rdname viewer
#' @export
viewImages <- function (images, colourScales = NULL, point = NULL, interactive = TRUE, crosshairs = TRUE, orientationLabels = TRUE, infoPanel = defaultInfoPanel, ...)
{
    if (is.list(images))
        images <- lapply(images, as, "MriImage")
    else
        images <- list(as(images, "MriImage"))
    
    nImages <- length(images)
    
    if (is.null(colourScales))
        colourScales <- rep(list(1), nImages)
    else if (!is.list(colourScales))
        colourScales <- as.list(colourScales)
    else if (length(colourScales) != nImages)
        colourScales <- rep(colourScales, length.out=nImages)
    
    layers <- lapply(seq_len(nImages), function(i) {
        layer <- lyr(images[[i]], colourScales[[i]]$colours)
        layer$label <- basename(images[[i]]$getSource())
    })
    
    do.call(RNifti::view, c(layers, list(point=point, interactive=interactive, crosshairs=crosshairs, labels=orientationLabels, infoPanel=infoPanel)))
}
