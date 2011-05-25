\name{MriImage-class}
\Rdversion{1.1}
\docType{class}
\alias{MriImage-class}
\alias{MriImage}

\title{Class "MriImage"}
\description{
  This class represents an MRI image. An object of this class is made up of some voxel data, stored as a numeric array, and an \code{\link{MriImageMetadata}} object, which stores extra information about the image, such as the file it was read from, the voxel dimensions, and so on. Since the class inherits from \code{\link{MriImageMetadata}}, any object can be treated as an object of that class where needed. The group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} are defined for this class, although there are some limitations (see Details).
}
\section{Extends}{
Class \code{"\linkS4class{MriImageMetadata}"}, directly.
Class \code{"\linkS4class{SerialisableObject}"}, by class "MriImageMetadata", distance 2.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\section{Methods}{
  \describe{
    \item{\code{initialize(data, metadata)}:}{Create a new object of this class.}
    \item{\code{getData()}:}{Retrieve the array of voxel values.}
    \item{\code{getDataAtPoint(...)}:}{Retrieve the value of the voxel at the location specified by \code{c(...)}. Returns \code{NA} if the location is out of bounds.}
    \item{\code{getMetadata()}:}{Retrieve the embedded \code{\link{MriImageMetadata}} object.}
  }
}
\details{
  \code{Ops.image.mri} uses \code{\link{newMriImageWithBinaryFunction}}, and as such does not work with unary operators (unary \code{+} and \code{-}, and \code{!}). It is also not currently possible to use anything other than another MriImage object as the second argument. That is, one can add two MriImages together, but not add 10 to the value of each voxel in a single image (an error will be produced). This may be allowed in future.
  
  \code{Summary.image.mri} currently works only for a single image argument. A call such as \code{max(image1,image2)} will produce an error.
}
\author{Jon Clayden}
\seealso{The \code{\link{SerialisableObject}} and \code{\link{MriImageMetadata}} classes, which this class extends. Also the group generic functions \code{\link{Math}}, \code{\link{Ops}} and \code{\link{Summary}} (in the \code{base} package), and \code{\link{newMriImageWithBinaryFunction}}.}
\keyword{classes}
