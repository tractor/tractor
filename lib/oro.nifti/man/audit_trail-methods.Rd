% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{audit.trail-methods}
\docType{methods}
\alias{audit.trail-methods}
\alias{audit.trail,nifti-method}
\alias{audit.trail}
\alias{audit.trail<-,nifti-method}
\alias{audit.trail<-}
\title{Extract or Replace NIfTI Audit Trail}
\description{
  Operators that act on the audit trail (XML) in the NIfTI header.
}
\usage{
\S4method{audit.trail}{nifti}(object)
}
\arguments{
  \item{object}{is of class \code{nifti}.}
}
\section{Methods}{
  \describe{
    \item{object = "nifti"}{Extract or replace NIfTI audit trail.}
  }
}
\author{Andrew Thornton \email{zeripath@users.sourceforge.net}}
\examples{
\dontrun{
## Sternberg Item Recognition Paradigm (SIRP) fMRI Study + XML
## Extension Data
URL <- "http://nifti.nimh.nih.gov/nifti-1/data/sirp_fmri_study_ver4.tar.gz"
download.file(URL, dest="sirp.tar.gz", quiet=TRUE)
fnames <- system("tar zxvf sirp.tar.gz", intern=TRUE)
sirp <- readNIfTI(fnames[1]) # newsirp_final_XML.nii
(sirp.xml <- xmlTreeParse(sirp@"extensions"[[1]]@"edata", asText=TRUE))
}
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
