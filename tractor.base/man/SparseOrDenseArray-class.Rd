\name{SparseOrDenseArray-class}
\Rdversion{1.1}
\docType{class}
\alias{SparseOrDenseArray}
\alias{SparseOrDenseArray-class}

\title{Class \code{"SparseOrDenseArray"}}
\description{
This virtual class encapsulates either a standard R \code{\link{array}}, or an object of class \code{\linkS4class{SparseArray}}. It is used as the data field in \code{\linkS4class{MriImage}} objects.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\examples{
showClass("SparseOrDenseArray")
}
\keyword{classes}
