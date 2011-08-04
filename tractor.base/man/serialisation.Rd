\name{serialisation}
\alias{isDeserialisable}
\alias{deserialiseReferenceObject}
\title{Reference object serialisation and deserialisation}
\description{
  Rather than using \R's \code{\link{save}} and \code{\link{load}} functions directly for reference objects, TractoR uses the \code{\linkS4class{SerialisableObject}} class and these functions to save and load objects. The main difference is that this approach stores only the data in the object, and not the functions which operate on them. This helps backward compatibility when new member functions are added.
}
\usage{
isDeserialisable(object, expectedClass = NULL)
deserialiseReferenceObject(file = NULL, object = NULL, raw = FALSE)
}
\arguments{
  \item{object}{A list object in (raw) serialised form. See Details.}
  \item{expectedClass}{A class name which the object is expected to inherit. Any class is acceptable if this parameter is \code{NULL}.}
  \item{file}{A file name to deserialise from.}
  \item{raw}{If \code{TRUE}, the raw serialised object is returned; otherwise the object is converted back to its original class.}
}
\details{
  The \code{serialise} member function of the \code{\link{SerialisableObject}} class can be used to create and/or \code{\link{save}} a version of an object which contains a hierarchical representation of the data embedded in it. These serialised objects are standard R lists, with an \code{"originalClass"} attribute describing the class of the original object. The functions listed above can be used to deserialise them.
  
  Note that this should generally NOT be used as the primary mechanism for saving and loading \code{\link{MriImage}} objects. Saving to standard NIfTI/Analyze format is usually preferable, and can be done using \code{\link{writeMriImageToFile}}.
}
\value{
  \code{isDeserialisable} returns \code{TRUE} if the \code{object} is deserialisable and inherits from the specified class. \code{deserialiseReferenceObject} returns a raw or reconstituted object after deserialisation.
}
\author{Jon Clayden}
\seealso{\code{\linkS4class{SerialisableObject}}, \code{\link{save}}, \code{\link{load}}, \code{\link{writeMriImageToFile}}.}
