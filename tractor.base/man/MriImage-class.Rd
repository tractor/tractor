\name{MriImage-class}
\Rdversion{1.1}
\docType{class}
\alias{MriImage}
\alias{MriImage-class}
\alias{[,MriImage,ANY,ANY-method}
\alias{[,MriImage,ANY,missing-method}
\alias{[,MriImage,missing,ANY-method}
\alias{[,MriImage,missing,missing-method}
\alias{[<-,MriImage,ANY,ANY-method}
\alias{[<-,MriImage,ANY,missing-method}
\alias{[<-,MriImage,missing,ANY-method}
\alias{[<-,MriImage,missing,missing-method}
\alias{Math.MriImage}
\alias{Ops.MriImage}
\alias{Summary.MriImage}
\alias{dim.MriImage}
\alias{as.array.MriImage}

\title{Class \code{"MriImage"}}
\description{
This class represents an MRI image. An object of this class is made up of some voxel data, stored as a sparse or dense numeric array, and an \code{\linkS4class{MriImageMetadata}} object, which stores extra information about the image, such as the file it was read from, the voxel dimensions, and so on. Since the class inherits from \code{\linkS4class{MriImageMetadata}}, any object can be treated as an object of that class where needed. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} are defined for this class, as are methods for coercing to and from a standard \R \code{\link{array}}.
}
\section{Extends}{
Class \code{"\linkS4class{MriImageMetadata}"}, directly.
Class \code{"\linkS4class{SerialisableObject}"}, by class "MriImageMetadata", distance 2.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "MriImage", i = "ANY", j = "ANY")}: Index methods for the \code{MriImage} class. These are wrappers around the corresponding method for the \code{array} or \code{SparseArray} classes, depending on the underlying data type.}
    \item{[}{\code{signature(x = "MriImage", i = "ANY", j = "missing")}}
    \item{[}{\code{signature(x = "MriImage", i = "missing", j = "ANY")}}
    \item{[}{\code{signature(x = "MriImage", i = "missing", j = "missing")}}
    \item{[<-}{\code{signature(x = "MriImage", i = "ANY", j = "ANY")}: Replacement methods for the \code{MriImage} class. These are wrappers around the corresponding method for the \code{array} or \code{SparseArray} classes, depending on the underlying data type.}
    \item{[<-}{\code{signature(x = "MriImage", i = "ANY", j = "missing")}}
    \item{[<-}{\code{signature(x = "MriImage", i = "missing", j = "ANY")}}
    \item{[<-}{\code{signature(x = "MriImage", i = "missing", j = "missing")}}
	 }
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\note{
The \code{Summary} group generic currently works only for a single image argument. A call such as \code{max(image1,image2)} will produce an error.
}
\seealso{
The \code{\linkS4class{SerialisableObject}} and \code{\linkS4class{MriImageMetadata}} classes, which this class extends. Also the group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} (in the \code{methods} package); \code{\link{newMriImageWithSimpleFunction}} and \code{\link{newMriImageWithBinaryFunction}}.
}
\examples{
showClass("MriImage")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{imagedims}:}{See the \code{MriImageMetadata} class.}
    \item{\code{voxdims}:}{See the \code{MriImageMetadata} class.}
    \item{\code{voxunit}:}{See the \code{MriImageMetadata} class.}
    \item{\code{source}:}{See the \code{MriImageMetadata} class.}
    \item{\code{datatype}:}{See the \code{MriImageMetadata} class.}
    \item{\code{origin}:}{See the \code{MriImageMetadata} class.}
    \item{\code{storedXform}:}{See the \code{MriImageMetadata} class.}
    \item{\code{tags}:}{See the \code{MriImageMetadata} class.}
    \item{\code{data}:}{The image data, stored as an object of class \code{SparseOrDenseArray}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(data, metadata, ...)}:}{Create a new object of this class.}
    \item{\code{summarise()}:}{Retrieve information about this object. This method is usually only called implicitly by the \code{"show"} method.}
    \item{\code{apply(...)}:}{Apply a function to margins of the image, as with the \code{\link{apply}} function in the base package.}
    \item{\code{getData()}:}{Retrieve the array of voxel values.}
    \item{\code{getDataAtPoint(...)}:}{Retrieve the value of the voxel at the location specified by \code{c(...)}. Returns \code{NA} if the location is out of bounds.}
    \item{\code{getMetadata()}:}{Retrieve the embedded \code{\link{MriImageMetadata}} object.}
    \item{\code{getNonzeroIndices(array=TRUE, positiveOnly=FALSE)}:}{Get the locations of nonzero voxels in the image.}
    \item{\code{getSparseness()}:}{Retrieve the proportion of image pixels or voxels which are nonzero.}
    \item{\code{isSparse()}:}{\code{TRUE} if the image data is stored as a \code{SparseArray} object; \code{FALSE} otherwise.}
  }

The following methods are inherited (from the corresponding class):
getFieldOfView ("MriImageMetadata"), getVoxelUnit ("MriImageMetadata"), getDimensions ("MriImageMetadata"), initialize ("MriImageMetadata"), getDimensionality ("MriImageMetadata"), isInternal ("MriImageMetadata"), getDataType ("MriImageMetadata"), getSource ("MriImageMetadata"), getTags ("MriImageMetadata"), serialise ("SerialisableObject"), getVoxelDimensions ("MriImageMetadata"), getStoredXformMatrix ("MriImageMetadata"), summarise ("MriImageMetadata"), getTag ("MriImageMetadata"), setSource ("MriImageMetadata"), getOrigin ("MriImageMetadata"), methods ("SerialisableObject")
}
