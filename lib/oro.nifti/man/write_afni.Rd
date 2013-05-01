% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{writeAFNI-methods}
\docType{methods}
\alias{writeAFNI-methods}
\alias{writeAFNI,afni-method}
\alias{writeAFNI,ANY-method}
\alias{writeAFNI}
\title{writeAFNI}
\description{
  This function saves a afni-class object to HEAD/BRIK pair in
  AFNI format.
}
\usage{
\S4method{writeAFNI}{afni}(nim, fname, verbose = FALSE, warn = -1)
}
\arguments{
  \item{nim}{is an object of class \code{afni}.}
  \item{fname}{is the path and file name to save the AFNI file
    (.HEAD/BRIK) \bold{without} the suffix.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{\link{options}} for more details.}
}
\section{Methods}{
  \describe{
    \item{nim = "afni"}{ Write AFNI volume to disk. }
    \item{nim = "ANY"}{ not implemented. }
  }
}
\details{
  The \code{writeAFNI} function utilizes the internal \code{writeBin}
  and \code{writeLines} command to write information to header/binary
  file pair.
  
  Current acceptable data types include
  \describe{
    \item{\dQuote{INT16}}{DT SIGNED SHORT (16 bits per voxel)}
    \item{\dQuote{FLOAT32}}{DT FLOAT (32 bits per voxel)}
    \item{\dQuote{COMPLEX128}}{DT COMPLEX (128 bits per voxel)}
    }
}
\value{
  Nothing.
}
\references{
  AFNI\cr
  \url{http://afni.nimh.nih.gov/pub/dist/src/README.attributes}
}
\seealso{
  \code{\link{writeANALYZE}}, \code{\link{writeNIfTI}}
}
\author{Karsten Tabelow \email{karsten.tabelow@wias-berlin.de}}
\examples{
## Taken from the AFNI Matlab Library
## http://afni.nimh.nih.gov/pub/dist/data/afni_matlab_data.tgz
afni.path <- system.file("afni", package="oro.nifti")
orig <- readAFNI(file.path(afni.path, "ARzs_CW_avvr.DEL+orig"))
writeAFNI(orig, "test-afni-image", verbose=TRUE)

data <- readAFNI("test-afni-image", verbose=TRUE)
image(orig, zlim=c(0.5,256), oma=rep(2,4), bg="white")
image(data, zlim=c(0.5,256), oma=rep(2,4), bg="white")
abs.err <- abs(data - orig)
image(as(abs.err, "nifti"), zlim=range(0,1), oma=rep(2,4),
      bg="white")
}
\keyword{file}
\keyword{methods}
