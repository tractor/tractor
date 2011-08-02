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
\examples{
showClass("SparseOrDenseArray")
}
\keyword{classes}
