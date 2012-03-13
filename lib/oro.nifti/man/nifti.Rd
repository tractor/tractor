% Part of the dcemri package for R
% Distributed under the BSD license: see dcemri/COPYING
%
% $Id: nifti.Rd 332 2010-01-29 16:54:07Z bjw34032 $

\name{nifti}
\Rdversion{1.1}
\alias{nifti}
\alias{is.nifti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for NIfTI}
\description{
  Constructor for NIfTI class objects.
}
\usage{
nifti(img = array(0, dim = rep(1, 4)), dim, datatype = 2,
      cal.min = NULL, cal.max = NULL, pixdim = NULL, ...)
is.nifti(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{is a multidimensional array of data.}
  \item{dim}{is the dimension of the data (default = \code{missing}).}
  \item{datatype}{is an integer that denotes the type of data contained
    in each voxel.  See \code{convert.datatype} or the NIfTI
    documentation for more details.}
  \item{cal.min}{allows user-specified minimum value in the array
    (visualization purposes only).}
  \item{cal.max}{allows user-specified minimum value in the array
    (visualization purposes only).}
  \item{pixdim}{allows user-specified pixel dimension vector (length = 8).}
  \item{\dots}{allows for additional \sQuote{slots} to be specified.}
  \item{x}{is an object to be checked.}
}
%\details{}
\value{
  An object of class \code{nifti}.
}
\references{
  NIfTI-1\cr
  \url{http://nifti.nimh.nih.gov/}
}
\author{
  Brandon Whitcher \email{bjw34032@users.sourceforge.net}
}
\seealso{
  \code{\linkS4class{nifti}}, \code{\link{anlz}},
  \code{\link{convert.datatype}}
}
\examples{
nim <- nifti() # default
nim
nim <- nifti(datatype=4) # 2-byte integers
nim
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
