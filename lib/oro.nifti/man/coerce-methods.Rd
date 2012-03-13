% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{coerce-methods}
\docType{methods}
\alias{coerce-methods}
\alias{coerce,array,anlz-method}
\alias{coerce<-,array,anlz-method}
\alias{as,array,anlz-method}
\alias{as<-,array,anlz-method}
\alias{as<-,array,nifti-method}
\alias{coerce,list,anlz-method}
\alias{coerce<-,list,anlz-method}
\alias{coerce,array,nifti-method}
\alias{as,array,nifti-method}
\alias{coerce,list,nifti-method}
\alias{coerce,anlz,nifti-method}
\alias{coerce<-,array,nifti-method}
\alias{as<-,array,nifti-method}
\alias{coerce<-,list,nifti-method}
\alias{coerce<-,anlz,nifti-method}
\title{Force an Object to Belong to the ANALYZE or NIfTI Class}
\description{
  Methods for function \code{coerce} in Package \sQuote{methods}.
}
\usage{
\S4method{as}{array,anlz}(object, Class)
\S4method{as}{array,anlz}(object, Class) <- value
\S4method{as}{array,nifti}(object, Class)
\S4method{as}{array,nifti}(object, Class) <- value
}
\arguments{
  \item{object}{is an object of class \code{array} or inherits from
    \code{array}.}
  \item{Class}{is the name of the class to which \sQuote{object} should
    be coerced; i.e., \code{nifti}.}
  \item{value}{is the values used to modify \sQuote{object} (see the
    discussion below).  You should supply an object with class
    \code{nifti} in order to pass NIfTI header information.}
}
\details{
  If 
}
\section{Methods}{
  \describe{
    \item{from = "anlz", to = "nifti"}{An object of class \code{anlz}
      is coerced into a NIfTI object.}
    \item{from = "array", to = "anlz"}{An object of class \code{array}
      is coerced into an ANALYZE object.}
    \item{from = "array", to = "nifti"}{An object of class \code{array}
      is coerced into a NIfTI object.}
    \item{from = "list", to = "anlz"}{All objects of class \code{array}
      in the list are coerced into ANALYZE objects.  All other objects
      are left alone.  The original list structure is retained.}
    \item{from = "list", to = "nifti"}{All objects of class \code{array}
      in the list are coerced into NIfTI objects.  All other objects are
      left alone.  The original list structure is retained.}
  }
}
%\references{}
\author{
  Andrew Thornton \email{zeripath@users.sourceforge.net} and
  Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
\seealso{
  \code{\link{as}}
}
%\examples{}
\keyword{methods}
