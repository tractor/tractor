\name{MriImage}
\alias{MriImage}
\alias{image.mri}
\alias{Math.image.mri}
\alias{Ops.image.mri}
\alias{Summary.image.mri}
\alias{isMriImage}
\title{Class representing an MRI image}
\description{
  This class represents an MRI image. An object of this class is made up of some voxel data, stored as a numeric array, and an \code{\link{MriImageMetadata}} object, which stores extra information about the image, such as the file it was read from, the voxel dimensions, and so on. Since the class inherits from \code{\link{MriImageMetadata}}, any object can be treated as an object of that class where needed. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} are defined for this class, although there are some limitations (see Details).
}
\usage{
isMriImage(object)
}
\arguments{
  \item{object}{Any object.}
}
\details{
  \code{Ops.image.mri} uses \code{\link{newMriImageWithBinaryFunction}}, and as such does not work with unary operators (unary \code{+} and \code{-}, and \code{!}). It is also not currently possible to use anything other than another MriImage object as the second argument. That is, one can add two MriImages together, but not add 10 to the value of each voxel in a single image (an error will be produced). This may be allowed in future.
  
  \code{Summary.image.mri} currently works only for a single image argument. A call such as \code{max(image1,image2)} will produce an error.
}
\value{
  A list object (see \code{\link{list.object}}) with class \code{c("image.mri", "metadata.image.mri", "list.object", "list")} and the following function elements.
  \item{getData()}{Retrieve the array of voxel values.}
  \item{getDataAtPoint(...)}{Retrieve the value of the voxel at the location specified by \code{c(...)}. Returns NA if the location is out of bounds.}
  \item{getMetadata()}{Retrieve the embedded \code{\link{MriImageMetadata}} object.}
  \code{isMriImage} returns TRUE if \code{object} is an MriImage object.
}
\author{Jon Clayden}
\seealso{\code{\link{list.object}} and \code{\link{metadata.image.mri}}, both of which the \code{image.mri} class extends. \code{\link{[.image.mri}} for extracting and replacing voxel data in an MriImage object. Also the group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} (in the \code{base} package), and \code{\link{newMriImageWithBinaryFunction}}.}
