% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{readAFNI}
\alias{readAFNI}
%\alias{read.afni.content}
\title{readAFNI}
\description{
  These functions read in the header information and multidimensional
  array from a binary file in AFNI format into a
  \code{\linkS4class{afni}}-class object.
}
\usage{
readAFNI(fname, vol = NULL, verbose = FALSE, warn = -1, call = NULL)
%read.afni.content(fname, vol, verbose = FALSE, warn = -1, call = NULL)
}
\arguments{
  \item{fname}{is the file name of the AFNI file.}
  \item{vol}{vector of brick numbers to be read from file.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regularegulatete the display of warnings
    (default = -1).  See \code{options} for more details.}
  \item{call}{keeps track of the current function call for use in the
    AFNI extension.}
}
\details{
  The \code{readAFNI} function utilizes internal methods \code{readBin}
  and \code{readLines} to efficiently extract information from the
  header and binary file(s).

  Current acceptable data types include
  \describe{
    \item{\dQuote{INT16}}{DT SIGNED SHORT (16 bits per voxel)}
    \item{\dQuote{FLOAT32}}{DT FLOAT (32 bits per voxel)}
    \item{\dQuote{COMPLEX128}}{DT COMPLEX (128 bits per voxel)}
    }
}
\value{
  object of class \code{\linkS4class{afni}}
}
\references{
  AFNI\cr
  \url{http://afni.nimh.nih.gov/pub/dist/src/README.attributes}
}
\author{ Karsten Tabelow \email{karsten.tabelow@wias-berlin.de} }
\seealso{
  \code{\link{readANALYZE}}, \code{\link{readNIfTI}}
}
\examples{
## Taken from the AFNI Matlab Library
## http://afni.nimh.nih.gov/pub/dist/data/afni_matlab_data.tgz
afni.path <- system.file("afni", package="oro.nifti")
orig <- readAFNI(file.path(afni.path, "ARzs_CW_avvr.DEL+orig"))
image(orig, zlim=c(0.5,256), oma=rep(2,4))
orthographic(orig, zlim=c(0.5,256), oma=rep(2,4))
}
\keyword{file}
\keyword{methods}
