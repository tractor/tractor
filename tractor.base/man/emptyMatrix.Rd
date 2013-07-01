\name{emptyMatrix}
\alias{emptyMatrix}
\alias{is.emptyMatrix}
\title{The empty matrix}
\description{
  The empty matrix is a standard matrix of dimensions 0 x 0. It is intended to be used as a placeholder where a matrix is required but no information is stored.
}
\usage{
emptyMatrix()
is.emptyMatrix(object)
}
\arguments{
  \item{object}{Any object.}
}
\value{
\code{emptyMatrix} returns the empty matrix, equivalent to \code{matrix(NA,0,0)}. \code{is.emptyMatrix} returns \code{TRUE} if its argument is identical to the empty matrix.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
