% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
% Time-stamp: <2010-02-12 10:25:18 (bjw34032)>
% $Id: $

\name{hotmetal}
\alias{hotmetal}
\title{Hot Metal Color Table}
\description{
  The hotmetal color table patterned after the one used in Matlab. 
}
\usage{
hotmetal(n=64)
}
\arguments{
  \item{n}{is the number of color levels (default = 64).}
}
\details{
  Based on the \code{tim.colors} function in the \pkg{fields} package.
  The \code{hotmetal} function has been modified to break any dependence
  on code in the \pkg{fields} package.  Spline interpolation
  (\code{interpSpline}) is used when the number of requested colors is
  not the default.
}
\value{
  A vector of character strings giving the colors in hexadecimal
  format.
}
\seealso{
  \code{\link{terrain.colors}}, \code{\link{tim.colors}},
  \code{\link{topo.colors}}
}
\examples{
hotmetal(10) 
image(outer(1:20,1:20,"+"), col=hotmetal(75), main="hotmetal")
}
\keyword{aplot}
