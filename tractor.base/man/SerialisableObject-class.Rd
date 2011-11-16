\name{SerialisableObject-class}
\Rdversion{1.1}
\docType{class}
\alias{SerialisableObject}
\alias{SerialisableObject-class}

\title{Class \code{"SerialisableObject"}}
\description{
This reference class extends the standard \code{"\linkS4class{envRefClass}"} class, adding a function for simple serialisation of the data fields of an object, and one for finding all of the methods available for an object. A serialised object may be deserialised using the \code{\link{deserialiseReferenceObject}} function.
}
\section{Extends}{
All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\examples{
showClass("SerialisableObject")
}
\keyword{classes}
\section{Class-Based Methods}{
  \describe{
    \item{\code{serialise(file)}:}{If the \code{file} argument is missing, serialises the object to a list containing the (named) fields of the object, with its \code{"originalClass"} attribute set to the class name of the original object. If the file argument is supplied, this object is saved to the corresponding file name using the standard \code{\link{save}} function.}
    \item{\code{methods()}:}{Retrieve a character vector describing the methods available to the object.}
  }
}
