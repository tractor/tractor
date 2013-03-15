\name{viewImages}
\alias{viewImages}
\alias{defaultInfoPanel}
\title{A simple interactive viewer for MriImage objects}
\description{The \code{viewImages} function provides a simple interactive viewer for \code{MriImage} objects. 3D and 4D images may be used.}
\usage{
viewImages(images, colourScales = NULL, point = NULL, interactive = TRUE,
             crosshairs = TRUE, orientationLabels = TRUE,
             infoPanel = defaultInfoPanel, ...)

defaultInfoPanel(point, data, imageNames)
}
\arguments{
  \item{images}{An \code{MriImage} object, or list of \code{MriImage} objects.}
  \item{colourScales}{A list of colour scales to use for each image, which will be recycled to the length of \code{images}. See \code{\link{getColourScale}} for details. The default is to use greyscale.}
  \item{point}{For \code{viewImages}, a length 3 integer vector giving the initial location of the crosshairs, in voxels. For \code{defaultInfoPanel}, the current location of the crosshairs.}
  \item{interactive}{A single logical value. If \code{TRUE}, the plot is interactive.}
  \item{crosshairs}{A single logical value. If \code{TRUE}, the crosshairs are displayed.}
  \item{orientationLabels}{A single logical value. If \code{TRUE}, orientation labels are displayed.}
  \item{infoPanel}{A function with at least three arguments, which must plot something to fill the bottom-right panel of the viewer after each change of crosshair location. The three mandatory arguments correspond to the current location in the image, the image values at that location, and the names of each image. The \code{defaultInfoPanel} function is a valid example.}
  \item{\dots}{Additional arguments to \code{infoPanel}.}
  \item{data}{A list giving the data value(s) at the current crosshair location in each image displayed.}
  \item{imageNames}{A character vector giving a name for each image displayed.}
}
\note{
The \code{defaultInfoPanel} function is not intended to be called directly. It is a simple example of a valid value for the \code{infoPanel} argument to \code{viewImages}.
}
\value{
These functions are called for their side effects.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{\code{\link{getColourScale}}}
