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
\seealso{
\code{\linkS4class{MriImage}}, \code{\linkS4class{SparseArray}}, \code{\link{array}}
}
