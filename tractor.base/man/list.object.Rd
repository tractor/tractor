\name{list.object}
\alias{list.object}
\alias{isListObject}
\alias{print.list.object}
\title{The list.object class}
\description{
  The list.object class is inherited by the \code{\link{MriImage}} and \code{\link{MriImageMetadata}} classes and takes the general form of a list of functions with associated frame containing data fields.
}
\usage{
isListObject(object)
print.list.object(x, ...)
}
\arguments{
  \item{object}{Any object.}
  \item{x, child, parent}{Objects of class \code{list.object}.}
}
\details{
  The list.object class is the basis for various other classes in the TractoR packages. The class itself inherits from \code{link{list}}. The \code{isListObject} function can be used to test whether a given object is a list object or not. \code{print.list.object} calls the \code{summarise} function within the list object if one is defined; otherwise it prints the result of calling \code{summary(object)}, which will give a list of functions in the object.
}
\value{
  \code{isListObject} returns TRUE if \code{object} is a list object. \code{print.list.object} is only called for its side-effect.
}
\author{Jon Clayden}
\seealso{At the time of writing, section 10.7 of "An Introduction to R", on scope, gives an example of the principle behind the object-as-a-list-of-functions approach used here.}
