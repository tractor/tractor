% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{orientation-methods}
\docType{methods}
\alias{qform-methods}
\alias{qform,nifti-method}
\alias{qform}
\alias{sform-methods}
\alias{sform,nifti-method}
\alias{sform}
\title{Extract NIfTI 3D Image Orientation}
\description{
  Methods that act on the \dQuote{qform} and \dQuote{sform} information
  in the NIfTI header.
}
\usage{
\S4method{qform}{nifti}(object)
\S4method{sform}{nifti}(object)
}
\arguments{
  \item{object}{is an object of class \code{nifti}.}
}
\section{Methods}{
  \describe{
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
sform(mniLR)
}
\keyword{methods}
