\name{SparseArray-class}
\Rdversion{1.1}
\docType{class}
\alias{SparseArray}
\alias{SparseArray-class}
\alias{[,SparseArray,ANY,ANY-method}
\alias{[<-,SparseArray,ANY,ANY-method}
\alias{dim.SparseArray}
\alias{as.array.SparseArray}

\title{Class \code{"SparseArray"}}
\description{
This class represents an array with any number of dimensions, in which a significant proportion of entries are zero. The coordinates of nonzero entries are stored along with their values, with all remaining entries assumed to be zero. Methods are provided to index into the array in the standard way, using matrix or vector indices; and for coercing between \code{SparseArray} objects and standard (dense) arrays.
}
\section{Extends}{
Class \code{"\linkS4class{SerialisableObject}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SparseArray", i = "ANY", j = "ANY")}: Index method for the \code{SparseArray} class.}
    \item{[<-}{\code{signature(x = "SparseArray", i = "ANY", j = "ANY")}: Replacement method for the \code{SparseArray} class.}
	 }
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\note{
Using the current ``coordinate list'' storage convention, there is rarely any data size benefit to storing an array in this form unless it is at least 75\% sparse. Since \code{SparseArray} is a reference class, however, there may be some benefit in terms of avoiding duplication of the array on modification.

Indexing with logical or character vectors is currently not supported.
}
\seealso{
\code{\link{array}}. The implementation of indexing owes lot to the \code{slam} package.
}
\examples{
showClass("SparseArray")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{data}:}{Object of class \code{ANY}.}
    \item{\code{coords}:}{Object of class \code{matrix}.}
    \item{\code{dims}:}{Object of class \code{integer}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(...)}:}{Create a new object of this class.}
    \item{\code{aperm(perm)}:}{Permute dimensions of the array.}
    \item{\code{apply(margin, fun, ...)}:}{Apply a function to the margins of the array. This function should be used in preference to \code{\link{apply}} from the base package, since the latter will convert the \code{SparseArray} to a dense array first, with a potentially major memory cost.}
    \item{\code{flip(dimsToFlip)}:}{Flip the image along the specified dimension(s).}
    \item{\code{getDimensions()}:}{Retrieve an integer vector giving the dimensions of the array.}
    \item{\code{getDimensionality()}:}{Retrieve an integer vector of length 1 giving the number of dimensions in the array.}
    \item{\code{getCoordinates()}:}{Retrieve the matrix of coordinates of nonzero entries, with each row corresponding to a nonzero entry.}
    \item{\code{getData()}:}{Retrieve the vector of nonzero data values.}
    \item{\code{setCoordinatesAndData(newCoords, newData)}:}{Update the coordinate matrix and data vector explicitly.}
    \item{\code{setDimensions(newDims)}:}{Change the dimensions of the array.}
  }

The following methods are inherited (from the corresponding class):
serialise ("SerialisableObject"), methods ("SerialisableObject")
}
