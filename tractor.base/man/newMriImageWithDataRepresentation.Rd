\name{newMriImageWithDataRepresentation}
\alias{newMriImageWithDataRepresentation}
\title{Convert between image data storage conventions}
\description{
This function returns a version of its first argument using the specified data representation.
}
\usage{
newMriImageWithDataRepresentation(image, representation = c("dense", "coordlist"))
}
\arguments{
  \item{image}{An \code{MriImage} object.}
  \item{representation}{A character string specifying the required data representation.}
}
\value{
A version of \code{image} with using the specified data representation, i.e. an \code{array} (with \code{representation="dense"}) or a \code{SparseArray} (with \code{representation="coordlist"}).
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. URL http://www.jstatsoft.org/v44/i08/.
}
\seealso{
\code{\linkS4class{MriImage}}, \code{\linkS4class{SparseArray}}, \code{\link{array}}
}
