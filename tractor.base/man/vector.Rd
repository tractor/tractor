\name{vector functions}
\alias{vectorLength}
\alias{vectorCrossProduct}
\alias{angleBetweenVectors}
\alias{resolveVector}
\title{Miscellaneous vector functions}
\description{These functions provide the (Euclidean) length of a vector, the vector cross product or angle between two vectors.}
\usage{
vectorLength(vector)
vectorCrossProduct(a, b)
angleBetweenVectors(v1, v2)
resolveVector(len, ...)
}
\arguments{
  \item{vector, v1, v2}{Numeric vectors of any length.}
  \item{a, b}{Numeric 3-vectors.}
  \item{len}{The expected length of the vector.}
  \item{\dots}{Elements of the vector, to be concatenated together.}
}
\value{
For \code{vectorLength}, the Euclidean norm or length of the specified vector, given by \code{sqrt(sum(vector^2))}. For \code{vectorCrossProduct}, the vector cross product of the two specified vectors; and for \code{angleBetweenVectors}, the angle (in radians) between the two specified vectors.

The \code{resolveVector} function concatenates the values given in \code{\dots}, and if the result is a vector of length \code{len} then it is returned. If not, \code{NULL} is returned.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. URL http://www.jstatsoft.org/v44/i08/.
}
\seealso{
\code{\link{crossprod}} for the matrix cross product.
}
