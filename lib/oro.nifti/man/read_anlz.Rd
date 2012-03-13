% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{readANALYZE}
\alias{readANALYZE}
%\alias{read.analyze.content}
\title{readANALYZE}
\description{
  These functions read in the header information and multi-dimensional
  array from a binary file in Analyze 7.5 format.
}
\usage{
readANALYZE(fname, verbose = FALSE, warn = -1)
%read.analyze.content(fname, gzipped = TRUE, verbose = FALSE, warn = -1)
}
\arguments{
  \item{fname}{Pathname of the Analyze pair of files .img and .hdr
    without the suffix.}
%  \item{gzipped}{Allows the import/export of compressed (.gz) files
%    (default = \code{TRUE}).}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{options} for more details.}
}
\details{
  The internal functions \code{readBin} and \code{rawToChar} are utilized
  in order to efficiently extract information from a binary file.  The
  types of data are limited to 1- and 2-byte integers, 4-byte floats and
  8-byte doubles.
}
\value{
  An object of class \code{anlz} is produced.
}
\references{
  ANALYZE 7.5\cr
  \url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}
}
\seealso{
  \code{\link{readNIfTI}}
}
\examples{
## avg152T1
anlz.path <- system.file("anlz", package="oro.nifti")
mni152 <- readANALYZE(file.path(anlz.path, "avg152T1"))
image(mni152, oma=rep(2,4))
orthographic(mni152, oma=rep(2,4))

\dontrun{
## 27 scans of Colin Holmes (MNI) brain co-registered and averaged
URL <- "http://imaging.mrc-cbu.cam.ac.uk/downloads/Colin/colin_1mm.tgz"
urlfile <- file.path(tempdir(), "colin_1mm.tgz")
download.file(URL, dest=urlfile, quiet=TRUE)
untar(urlfile, exdir=tempdir())
colin <- readANALYZE(file.path(tempdir(), "colin_1mm"))
image(colin, oma=rep(2,4))
orthographic(colin, oma=rep(2,4))
}
}
\author{
  Brandon Whitcher \email{bjw34032@users.sourceforge.net},\cr
  Volker Schmid \email{volkerschmid@users.sourceforge.net}
}
\keyword{file}
