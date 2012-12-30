% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{descrip-methods}
\docType{methods}
\alias{descrip-methods}
\alias{descrip,anlz-method}
\alias{descrip,nifti-method}
\alias{descrip<-,anlz-method}
\alias{descrip<-,nifti-method}
\alias{descrip}
\alias{descrip<-}
\title{Extract or Replace NIfTI/Analyze Description}
\description{
  Methods that act on the \dQuote{description} character string in the
  NIfTI or ANALYZE header.
}
\usage{
\S4method{descrip}{nifti}(object)
%\S4method{descrip}{anlz}(object)
}
\arguments{
  \item{object}{is an object of class \code{nifti} or \code{anlz}.}
}
\section{Methods}{
  \describe{
  \item{object = "anlz"}{Extract or replace Analyze description.}
  \item{object = "nifti"}{Extract or replace NIfTI description.}
  }
}
\author{Brandon Whitcher \email{bwhitcher@gmail.com}}
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
descrip(mniLR)
descrip(mniLR) <- paste(descrip(mniLR), version$version.string, sep="; ")
descrip(mniLR)
}
\keyword{methods}
