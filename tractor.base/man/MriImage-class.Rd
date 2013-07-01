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
This class represents an MRI image. An object of this class is made up of some voxel data, stored as a sparse or dense numeric array, and some metadata, such as the file it was read from, the voxel dimensions, and so on. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} are defined for this class, as are methods for coercing to and from a standard \R \code{\link{array}}.
}
\section{Extends}{
Class \code{"\linkS4class{SerialisableObject}"}, directly.

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
\seealso{
The \code{\linkS4class{SerialisableObject}} class, which this class extends. Also the group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} (in the \code{methods} package); \code{\link{newMriImageWithSimpleFunction}} and \code{\link{newMriImageWithBinaryFunction}}.
}
\examples{
showClass("MriImage")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{imageDims}:}{Object of class \code{integer}.}
    \item{\code{voxelDims}:}{Object of class \code{numeric}.}
    \item{\code{voxelDimUnits}:}{Object of class \code{character}.}
    \item{\code{source}:}{Object of class \code{character}.}
    \item{\code{origin}:}{Object of class \code{numeric}.}
    \item{\code{storedXform}:}{Object of class \code{matrix}.}
    \item{\code{reordered}:}{Object of class \code{logical}.}
    \item{\code{tags}:}{Object of class \code{list}.}
    \item{\code{data}:}{Object of class \code{MriImageData}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(imageDims=NULL, voxelDims=NULL, voxelDimUnits=NULL, source="", origin=NULL, storedXform=emptyMatrix(), reordered=TRUE, tags=list(), data=NULL, ...)}:}{Create a new object of this class.}
    \item{\code{summarise()}:}{Retrieve information about this object. This method is usually only called implicitly by the \code{"show"} method.}
    \item{\code{apply(...)}:}{Apply a function to margins of the image, as with the \code{\link{apply}} function in the base package.}
    \item{\code{getData()}:}{Retrieve the array of voxel values.}
    \item{\code{getDataAtPoint(...)}:}{Retrieve the value of the voxel at the location specified by \code{c(...)}. Returns \code{NA} if the location is out of bounds.}
    \item{\code{getDimensionality()}:}{Get the dimensionality of the image.}
    \item{\code{getDimensions()}:}{Get the number of pixels or voxels in each dimension.}
    \item{\code{getFieldOfView()}:}{Get the field of view.}
    \item{\code{getMetadata()}:}{Retrieve a version of the object with the \code{data} field set to \code{NULL}.}
    \item{\code{getNonzeroIndices(array=TRUE, positiveOnly=FALSE)}:}{Get the locations of nonzero voxels in the image.}
    \item{\code{getOrigin()}:}{Get the coordinates of the image origin, in voxels.}
    \item{\code{getSource()}:}{Get the source file name associated with the image.}
    \item{\code{getSparseness()}:}{Retrieve the proportion of image pixels or voxels which are nonzero.}
    \item{\code{getStoredXformMatrix()}:}{Get the xform matrix that was read with the image, if any. This will be the empty matrix (see \code{\link{emptyMatrix}}) if none is stored.}
    \item{\code{getTag(key)}:}{Get the tag value associated with the specified key.}
    \item{\code{getTags()}:}{Get the list of tags associated with the image.}
    \item{\code{getVoxelDimensions()}:}{Get the size of the voxels in each dimension.}
    \item{\code{getVoxelUnits()}:}{Get the spatial and temporal units relating to the voxel dimensions.}
    \item{\code{setOrigin(newOrigin)}:}{Set the image origin.}
    \item{\code{setSource(newSource)}:}{Set the source file associated with the image.}
    \item{\code{isEmpty()}:}{\code{TRUE} if no data is stored with the image.}
    \item{\code{isInternal()}:}{\code{TRUE} if no source file is associated with the image.}
    \item{\code{isReordered()}:}{\code{TRUE} if the image data has been reordered to LAS.}
    \item{\code{isSparse()}:}{\code{TRUE} if the image data is stored as a \code{SparseArray} object, \code{NA} if there is no data stored with the image, and \code{FALSE} otherwise.}
  }

The following methods are inherited (from the corresponding class):
fields ("SerialisableObject"), serialise ("SerialisableObject"), methods ("SerialisableObject")
}
