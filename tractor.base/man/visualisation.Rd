\name{visualisation}
\alias{createCombinedGraphics}
\alias{createProjectionGraphic}
\alias{createSliceGraphic}
\alias{createContactSheetGraphic}
\title{Visualise MriImage objects}
\description{
  Visualise \code{MriImage} objects.
}
\usage{
createCombinedGraphics(images, modes, colourScales, axes = 1:3, sliceLoc = NULL, device = c("internal", "png"), alphaImages = NULL, prefix = "image", zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
createProjectionGraphic(image, axis, device = c("internal", "png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
createSliceGraphic(image, x = NA, y = NA, z = NA, device = c("internal", "png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
createContactSheetGraphic(image, axis, device = c("internal", "png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
}
\arguments{
  \item{images}{A list of \code{\link{MriImage}} objects.}
  \item{image}{A single \code{\link{MriImage}} object.}
  \item{modes}{A character vector of the same length as \code{images}, each element being "slice" or "projection" (or abbreviations), indicating which type of visualisation should be applied to each image.}
  \item{colourScale, colourScales}{A single colour scale definition, or a list in the plural case. See Details.}
  \item{axis, axes}{A vector of axes along which slice/projection images should be created. 1 is left-right, 2 is anterior-posterior, 3 is superior-inferior.}
  \item{x, y, z}{Integer vectors, each of length 1. Exactly one of these must be specified to indicate the plane of interest.}
  \item{sliceLoc}{Like \code{x}, \code{y} and \code{z}, except that a point in 3 dimensions is specified. Must not be \code{NA} for each \code{axis} requested.}
  \item{device}{Either "internal" for display on the default graphics device, or "png" for creating PNG format image file(s). Abbreviations are fine.}
  \item{alphaImages}{A list of \code{\link{MriImage}} objects to be used as transparency masks. Must be the same length as \code{images} if not \code{NULL}. \code{NULL} values in the list indicate no mask.}
  \item{prefix, file}{A file name or prefix (to which "axial", "coronal" or "sagittal" will be added) to be used when \code{device} is "png".}
  \item{zoomFactor}{Factor by which to enlarge the image. Applies only when \code{device} is "png".}
  \item{filter}{Interpolation filter to be used by ImageMagick.}
  \item{windowLimits}{Numeric vector of length 2 giving the limits of the colour scale, or \code{NULL} for limits matching the range of the image data. Passed as the \code{zlim} argument to \code{\link{image}}.}
  \item{clearance}{Number of voxels' clearance to leave around each slice image in the contact sheet. Passed to \code{\link{newMriImageByTrimming}}.}
  \item{nColumns}{Number of slices per row in the contact sheet grid. If \code{NULL}, the function will aim for a square grid.}
  \item{add}{Overlay the graphic on a previous one. Used only when \code{device} is "internal".}
}
\details{
  These functions create 2D visualisations of 3D images by slicing or maximum intensity projection.
  
  Colour scales can be specified in any of three ways. Firstly, by a single number, representing a predefined colour scale. Currently valid values are 1 (greyscale, black background), 2 (red to yellow heat scale, red background), 3 (blue to red rainbow scale, blue background) and 4 (blue to white to red diverging scale, white background). Secondly, a single colour name can be given (see \code{\link{colours}}); in this case the background will be black. This is useful for binary images. Thirdly and most flexibly, a list with two named elements can be given: \code{colours}, a vector of colours representing the colour scale, perhaps created using \code{\link{rgb}}; and \code{background}, a single colour representing the background.

  NB: When the \code{device} option is set to "png", ImageMagick is required by these functions.
}
\value{
  These functions are called for their side effects.
}
\author{Jon Clayden}
\seealso{\code{\link{image}}, \code{\link{colours}}, \code{\link{rgb}}}
