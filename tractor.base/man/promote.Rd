\name{promote}
\alias{promote}
\title{Promote a vector to a single-column or single-row matrix}
\description{
The \code{promote} function promotes a vector argument to a single-column or single-row matrix. Matrix arguments are returned unmodified.
}
\usage{
promote(x, byrow = FALSE)
}
\arguments{
  \item{x}{A vector or matrix.}
  \item{byrow}{Logical value: if \code{TRUE}, a vector will be promoted to a single-row matrix; otherwise a single-column matrix will result.}
}
\value{
A matrix version of the \code{x} argument.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. URL http://www.jstatsoft.org/v44/i08/.
}
\seealso{
\code{\link{matrix}}
}
