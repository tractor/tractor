\name{list.object}
\alias{list.object}
\alias{isListObject}
\alias{print.list.object}
\title{The list.object class}
\description{
  The list.object class is inherited by the \code{\link{MriImage}} and \code{\link{MriImageMetadata}} classes and takes the general form of a list of functions with associated frame containing data fields. \code{isListObject} tests for this class, and \code{print.list.object} prints an object of this class.
}
\usage{
isListObject(object)
\S3method{print}{list.object}(x, ...)
}
\arguments{
  \item{object}{Any object.}
  \item{x}{Object of class \code{list.object}.}
  \item{...}{Further options to \code{print} (though none are supported for this type of object).}
}
\details{
  The list.object class is the basis for various other classes in the TractoR packages. The class itself inherits from \code{\link{list}}. The \code{isListObject} function can be used to test whether a given object is a list object or not. \code{print.list.object} calls the \code{summarise} function within the list object if one is defined; otherwise it prints the result of calling \code{summary(object)}, which will give a list of functions in the object.
}
\value{
  \code{isListObject} returns TRUE if \code{object} is a list object. \code{print.list.object} is only called for its side-effect.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{At the time of writing, section 10.7 of "An Introduction to R", on scope, gives an example of the principle behind the object-as-a-list-of-functions approach used here.}
