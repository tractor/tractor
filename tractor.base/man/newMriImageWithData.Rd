\encoding{UTF-8}
\name{newMriImageWithData}
\alias{newMriImageWithData}
\alias{newMriImageAsShapeOverlay}
\alias{newMriImageByExtraction}
\alias{newMriImageByMasking}
\alias{newMriImageByThresholding}
\alias{newMriImageByTrimming}
\alias{newMriImageFromTemplate}
\alias{newMriImageWithBinaryFunction}
\alias{newMriImageWithSimpleFunction}
\alias{generateImageDataForShape}

\title{Functions for creating MriImage objects from data}
\description{
  Functions for creating MriImage objects from data, including other images.
}
\usage{
newMriImageWithData(data, metadata)
newMriImageFromTemplate(image, ...)

newMriImageByExtraction(image, dim, loc)
newMriImageByMasking(image, mask)
newMriImageByThresholding(image, level, defaultValue = 0)
newMriImageByTrimming(image, clearance = 4)

newMriImageAsShapeOverlay(type = c("cross", "block"), baseImage, ...)
generateImageDataForShape(type = c("cross", "block"), dim, background = 0, centre = NA, width = NA)

newMriImageWithSimpleFunction(image, fun, ..., newDataType = NULL)
newMriImageWithBinaryFunction(image1, image2, fun, ..., newDataType = NULL)
}
\arguments{
  \item{data}{An array of voxel data.}
  \item{metadata}{An \code{MriImageMetadata} object.}
  \item{image, image1, image2}{\code{MriImage} objects.}
  \item{dim, loc}{For \code{newMriImageByExtraction}, the dimension and location along that dimension for which data should be extracted. For \code{generateImageDataForShape}, \code{dim} is the dimensions of the image. \code{newMriImageAsShapeOverlay} takes this from the \code{baseImage}.}
  \item{mask}{An array of mode \code{logical} indicating which voxels are in the mask. Must have the same dimensions as the image.}
  \item{level}{A numeric value specifying the threshold level.}
  \item{defaultValue}{The value of the final image in voxels which are below threshold.}
  \item{clearance}{The number of voxels' clearance left around a trimmed image.}
  \item{type}{The shape type to generate. A "block" is a cubic region of the image; a "cross" is the central line of the cube in each dimension.}
  \item{baseImage}{The \code{MriImage} to use as a base for the overlay.}
  \item{background}{The voxel value outside the shape.}
  \item{centre, width}{The centre and width of the shape.}
  \item{newDataType}{The data type of the new image. If \code{NULL}, then the data type is the same as the source image.}
  \item{fun}{A function object, taking one or two numeric array parameters, as appropriate.}
  \item{\dots}{For \code{newMriImageFromTemplate}, further parameters to \code{\link{newMriImageMetadataFromTemplate}}. For \code{newMriImageAsShapeOverlay}, further parameters to \code{generateImageDataForShape}. And for \code{newMriImageWithSimpleFunction} and \code{newMriImageWithBinaryFunction}, further parameters to \code{fun}.}
}
\details{
  All of these functions use data from arrays or \code{MriImage} objects to create a new \code{MriImage} object. \code{newMriImageWithData} is the basic function for creating an object from its constituents: an array of voxel values and an \code{MriImageMetadata} object. \code{newMriImageFromTemplate} takes an existing image, with its voxel data, and creates a new image, possibly with modifications to the metadata.
  
  \code{newMriImageByExtraction} reduces the dimensionality of the source image by one, by extracting a single "line" of data along one dimension. \code{newMriImageByMasking} modifies the data by masking out unwanted voxels, and \code{newMriImageByThresholding} by thresholding. \code{newMriImageByTrimming} trims empty space from the edges of an image, reducing the dimensions of the image and thus avoiding the storage of lots of zeroes. \code{newMriImageAsShapeOverlay} creates an image which contains a simple shape. \code{newMriImageWithSimpleFunction} and \code{newMriImageWithBinaryFunction} modify the image data by applying an arbitrary function to it. Any function that can be applied to numeric arrays, and expects one or two arguments, respectively, is suitable for \code{fun}.
}
\value{
  An \code{MriImage} object.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{\code{\link{MriImage}}}
