% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{aux.file-methods}
\docType{methods}
\alias{aux.file-methods}
\alias{aux.file,anlz-method}
\alias{aux.file,nifti-method}
\alias{aux.file}
\alias{aux.file<-,nifti-method}
\alias{aux.file<-,anlz-method}
\alias{aux.file<-}
\title{Extract or Replace NIfTI/Analyze Auxiliary File}
\description{
  Methods that act on the \dQuote{auxiliary file} character string in
  the NIfTI or Analyze header.
}
\usage{
\S4method{aux.file}{nifti}(object)
\S4method{aux.file}{anlz}(object)
}
\arguments{
  \item{object}{is an object of class \code{nifti} or \code{anlz}.}
}
\section{Methods}{
  \describe{
    \item{object = "anlz"}{Extract or replace Analyze auxiliary file.}
    \item{object = "nifti"}{Extract or replace NIfTI auxiliary file.}
  }
}
\author{ Brandon Whitcher \email{bwhitcher@gmail.com} }
\examples{
\dontrun{
url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_RL_nifti.nii.gz"
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "mniRL.nii.gz")
download.file(url, urlfile, quiet=TRUE)
}
options("niftiAuditTrail"=FALSE)

urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "mniRL.nii.gz")
mniRL <- readNIfTI(urlfile)
aux.file(mniRL)
aux.file(mniRL) <- "avg152T1_RL_nifti"
aux.file(mniRL)
}
\keyword{methods}
