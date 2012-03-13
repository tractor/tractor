% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
% 
% $Id: $

\name{tim.colors}
\alias{tim.colors}
\title{Tim's Useful Color Table}
\description{
  A pleasing rainbow style color table patterned after that used in 
  Matlab.
}
\usage{
tim.colors(n=64)
}
\arguments{
  \item{n}{is the number of color levels (default = 64).}
}
\details{
  Based on the \code{tim.colors} function in the \pkg{fields} package.
  The \code{tim.colors} function here has been modified to break any
  dependence on code in the \pkg{fields} package.  Spline interpolation
  (\code{interpSpline}) is used when the number of requested colors is
  not the default.
}
\value{
  A vector of character strings giving the colors in hexadecimal
  format.
}
\seealso{
  \code{\link{hotmetal}}, \code{\link{topo.colors}},
  \code{\link{terrain.colors}}
}
\author{
   Tim Hoar (GSP-NCAR); modified by B. Whitcher
}
\examples{
tim.colors(10) 
image(outer(1:20, 1:20, "+"), col=tim.colors(75), main="tim.colors")
}
\keyword{aplot}
