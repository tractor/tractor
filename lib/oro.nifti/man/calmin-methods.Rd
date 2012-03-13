% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{cal.min-methods}
\docType{methods}
\alias{cal.min-methods}
\alias{cal.min,anlz-method}
\alias{cal.min,nifti-method}
\alias{cal.min<-,anlz-method}
\alias{cal.min<-,nifti-method}
\alias{cal.min}
\alias{cal.min<-}
\alias{cal.max-methods}
\alias{cal.max,anlz-method}
\alias{cal.max,nifti-method}
\alias{cal.max<-,anlz-method}
\alias{cal.max<-,nifti-method}
\alias{cal.max}
\alias{cal.max<-}
\title{Extract or Replace NIfTI/Analyze Min or Max Values}
\description{
  Methods that act on the \sQuote{cal.min} and \sQuote{cal.max} numeric
  value in the NIfTI or ANALYZE header.
}
\usage{
\S4method{cal.min}{nifti}(object)
\S4method{cal.max}{nifti}(object)
%\S4method{cal.min}{anlz}(object)
%\S4method{cal.max}{anlz}(object)
}
\arguments{
  \item{object}{is an object of class \code{nifti} or \code{anlz}.}
}
\section{Methods}{
  \describe{
  \item{object = "anlz"}{Extract or replace the ANALYZE "cal_min" or
    "cal_max" value.}
  \item{object = "nifti"}{Extract or replace the NIfTI "cal_min" or
    "cal_max" value.}
  }
}
\author{Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
\examples{
\dontrun{
url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_LR_nifti.nii.gz"
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "mniLR.nii.gz")
download.file(url, urlfile, quiet=TRUE)
}
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "mniLR.nii.gz")
mniLR <- readNIfTI(urlfile)
cal.min(mniLR)
cal.max(mniLR)
}
\keyword{methods}
