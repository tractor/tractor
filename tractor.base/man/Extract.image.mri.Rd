\name{[.image.mri}
\alias{[.image.mri}
\alias{[<-.image.mri}
\title{Extract or replace subsets of the data in an MriImage object}
\description{
  Extract or replace subsets of the data in an MriImage object.
}
\usage{
\S3method{[}{image.mri}(x, ...)
\S3method{[}{image.mri}(x, ...) <- value
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
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{\code{\link{[}}}
