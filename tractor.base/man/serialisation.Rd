\name{serialisation}
\alias{deserialiseMriImage}
\alias{deserialiseMriImageMetadata}
\alias{isDeserialisable}
\alias{serialiseListObject}
\title{List object serialisation and deserialisation}
\description{
  Rather than using \R's \code{\link{save}} and \code{\link{load}} functions directly for list objects, TractoR uses these functions to save and load objects of this type. The main difference is that \code{serialiseListObject} stores only the data in the object, and not the functions which operate on them. This helps backward compatibility when new member functions are added.
}
\usage{
serialiseListObject(object, file = NULL)

isDeserialisable(object, expectedClass = "list.object")
deserialiseMriImage(file = NULL, object = NULL)
deserialiseMriImageMetadata(file = NULL, object = NULL)
}
\arguments{
  \item{object}{For \code{serialiseListObject}, an object inheriting from class "list.object". For the other functions, a list object in serialised form (see Details).}
  \item{file}{A file name to serialise to, or deserialise from.}
  \item{expectedClass}{A class name which the object is expected to inherit.}
}
\details{
  The \code{serialiseListObject} function can be used to create and/or \code{\link{save}}() a version of a list object which contains a hierarchical representation of the data embedded in it. These serialised objects are of class "list", and contain an "originalClass" attribute describing the class of the original list object. The other functions listed above can be used to deserialise them.
  
  Note that this should generally NOT be used as the primary mechanism for saving and loading MriImage objects. Saving to standard NIfTI/Analyze format is usually preferable, and can be done using \code{\link{writeMriImageToFile}}.
}
\value{
  If \code{file} is NULL, \code{serialiseListObject} returns a serialised object ready to be passed to \code{save}. Otherwise it saves the object to the specified file and returns nothing. \code{isDeserialisable} returns TRUE if the \code{object} is deserialisable and inherits from the specified class.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{\code{\link{save}}, \code{\link{load}}, \code{\link{list.object}}, \code{\link{writeMriImageToFile}}.}
