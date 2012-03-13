% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{anlz}
\Rdversion{1.1}
\alias{anlz}
\alias{is.anlz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for Analyze}
\description{
  Constructor for Analyze class objects.
}
\usage{
anlz(img = array(0, dim = rep(1, 4)), dim, datatype = 2, ...)
is.anlz(x)
}
\arguments{
  \item{img}{is a multidimensional array of data.}
  \item{dim}{is the dimension of the data (default = \code{missing}).}
  \item{datatype}{is an integer that denotes the type of data contained
    in each voxel.  See \code{convert.datatype.anlz} or the ANALYZE
    documentation for more details.}
  \item{\dots}{allows for additional \sQuote{slots} to be specified.}
  \item{x}{is an object to be checked.}
}
%\details{}
\value{
  An object of class \code{anlz}.
}
\references{
  ANALYZE 7.5\cr
  \url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}
}
\author{ Brandon Whitcher \email{bjw34032@users.sourceforge.net} }
\seealso{
  \code{\linkS4class{anlz}}, \code{\linkS4class{nifti}},
  \code{\link{nifti}}, \code{\link{convert.datatype.anlz}}
}
\examples{
(aim <- anlz()) # default
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
