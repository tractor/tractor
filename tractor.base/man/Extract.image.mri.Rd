\name{[.image.mri}
\alias{[.image.mri}
\alias{[<-.image.mri}
\title{Extract or replace subsets of the data in an MriImage object}
\description{
  Extract or replace subsets of the data in an MriImage object.
}
\usage{
x[...]
x[...] <- value
}
\arguments{
  \item{x}{An object inheriting from class \code{image.mri}.}
  \item{\dots}{Elements to extract or replace. Any indexing method suitable for the underlying array can be used: see \code{\link{[}} for details.}
  \item{value}{An array of new voxel values.}
}
\value{
  For the extraction form, a vector of values extracted from the relevant locations in the image. The effect in this case is identical to \code{x$getData()[...]}. For the replacement form, the returned value is an MriImage object identical to the original one, except with the data array modified appropriately, and with the \code{source} field set to "internal".
}
\author{Jon Clayden}
\seealso{\code{\link{[}}}
