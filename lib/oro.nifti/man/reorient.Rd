% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{reorient}
\alias{reorient}
\alias{inverseReorient}
\title{reorient}
\description{
  Transforms in the NIfTI header are parsed and normalized versions of
  these transforms are applied.
}
\usage{
reorient(nim, data, verbose = FALSE, invert = FALSE, tol = 1e-07)
inverseReorient(nim, verbose = FALSE)
}
\arguments{
  \item{nim}{is an object of class \code{nifti}.}
  \item{data}{is an array associated with \code{nim}.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{invert}{stores the inverse transform.}
  \item{tol}{is a very small value used to judge if a number is
    essentially zero.}
}
\details{
  This function utilizes the \code{performPermutation} function
  internally.
}
%\value{}
%\references{}
\seealso{
  \code{\link{performPermutation}}
}
%\examples{}
\author{Andrew Thornton \email{zeripath@users.sourceforge.net} and
  Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
%\keyword{}
