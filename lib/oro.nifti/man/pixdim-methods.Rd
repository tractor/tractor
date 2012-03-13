% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{pixdim-methods}
\docType{methods}
\alias{pixdim-methods}
\alias{pixdim,anlz-method}
\alias{pixdim,nifti-method}
\alias{pixdim<-,anlz-method}
\alias{pixdim<-,nifti-method}
\alias{pixdim}
\alias{pixdim<-}
\title{Extract or Replace NIfTI/Analyze Pixel Dimensions}
\description{
  Methods that act on the \dQuote{pixdim} numeric vector in the NIfTI or
  ANALYZE header.
}
\usage{
\S4method{pixdim}{nifti}(object)
%\S4method{pixdim}{anlz}(object)
}
\arguments{
  \item{object}{is an object of class \code{nifti} or \code{anlz}.}
}
\section{Methods}{
  \describe{
  \item{object = "anlz"}{Extract or replace ANALYZE "pixdim" value.}
  \item{object = "nifti"}{Extract or replace NIfTI "pixdim" value.}
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
pixdim(mniLR)
}
\keyword{methods}
