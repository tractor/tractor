% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{Convert Between fmridata and oro.nifti}
\alias{oro2fmri}
\alias{fmri2oro}
\title{Convert Between fmridata and oro.nifti Objects}
\description{
  NIfTI data can be converted between \code{fmridata} S3 objects
  (from the \pkg{fmri} package) and \code{nifti} S4 objects.
}
\usage{
oro2fmri(from, value = NULL, level = 0.75, setmask = TRUE)
fmri2oro(from, value = NULL, verbose = FALSE, reorient = FALSE,
         call = NULL) 
}
\arguments{
  \item{from}{is the object to be converted.}
  \item{value}{\code{NULL}}
  \item{level}{is the quantile level defining the mask.}
  \item{setmask}{is a logical variable (default = \code{TRUE}), whether
    to define a suitable mask based on \code{level}.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{reorient}{is a logical variable (default = \code{TRUE}) that
    enforces Qform/Sform transformations.}
  \item{call}{keeps track of the current function call for use in the
    NIfTI extension.}
}
\details{
  These functions enhance the capabilities of \pkg{fmri} by allowing the
  exchange of data objects between \code{nifti} and \code{fmridata}
  classes.
}
\value{
  The function \code{oro2fmri} produces an S3 object of class
  \code{fmridata}.  The function \code{fmri2oro} produces an S4
  object of class \code{nifti}.
}
%\references{}
\seealso{
  \code{\link{readNIfTI}}, \code{\link[fmri]{read.NIFTI}}
}
%\examples{}
\author{ Brandon Whitcher \email{bwhitcher@gmail.com} }
\keyword{misc}
