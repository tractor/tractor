\name{newSparseArrayWithData}
\alias{newSparseArrayWithData}
\title{Create a SparseArray object}
\description{
This function creates a \code{\linkS4class{SparseArray}} object from its constituent parts.
}
\usage{
newSparseArrayWithData(data, coordinates, dims)
}
\arguments{
  \item{data}{A vector of (nonzero) array elements.}
  \item{coordinates}{A matrix with as many rows as \code{data} has elements, containing the coordinates of each nonzero element in the array.}
  \item{dims}{The dimensions of the array.}
}
\value{
A \code{\linkS4class{SparseArray}} object.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
