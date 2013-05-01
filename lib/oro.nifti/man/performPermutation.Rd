% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{performPermutation}
\alias{performPermutation}
\title{performPermutation}
\description{
  Given an orthogonal permutation matrix \eqn{T}, an array of dimensions
  and a one-dimensional representation of data.  It will return a
  transformed array with the transformed dimensions.
}
\usage{
performPermutation(T, real.dimensions, data, verbose=FALSE)
}
\arguments{
  \item{T}{is an orthogonal matrix.}
  \item{real.dimensions}{is a one-dimensional array, representing the
    length of dimensions in data.}
  \item{data}{is a one-dimensional representation of the data to be
    transformed.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
}
\details{
  This function is mainly used by the \code{\link{reorient}} function to
transform nifti data into neuroradiological convention.
}
%\value{}
%\references{}
\seealso{\code{\link{reorient}},\code{\link{inverseReorient}}}
%\examples{}
\author{Andrew Thornton \email{zeripath@users.sourceforge.net}}
%\keyword{}
