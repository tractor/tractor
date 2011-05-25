\name{nilObject}
\alias{nilObject}
\alias{is.nilObject}
\title{The nil object}
\description{
  The nil object is an empty object of class \code{\link{SerialisableObject}}. It can be used as a placeholder where such an object of this class, or one of its subclasses, is required. It serialises to the empty list.
}
\usage{
nilObject()
is.nilObject(object)
}
\arguments{
  \item{object}{Any object.}
}
\value{
\code{nilObject} returns the nil object. \code{is.nilObject} returns \code{TRUE} if its argument is identical to the nil object, or if it is equivalent in the sense of serialising to an identical result.
}
\author{Jon Clayden}
\seealso{\code{\link{SerialisableObject}}}
