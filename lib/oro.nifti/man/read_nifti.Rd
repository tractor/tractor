% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{readNIfTI}
\alias{readNIfTI}
%\alias{read.nifti.content}
\title{readNIfTI}
\description{
  These functions read in the header information and multidimensional
  array from a binary file in NIfTI-1 format into a
  \code{\linkS4class{nifti}}-class object.
}
\usage{
readNIfTI(fname, verbose=FALSE, warn=-1, reorient=TRUE, call=NULL)
%read.nifti.content(fname, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
%                   warn=-1, reorient=FALSE, call=NULL)
}
\arguments{
  \item{fname}{is the file name of the NIfTI file(s).}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{options} for more details.}
  \item{reorient}{is a logical variable (default = \code{TRUE}) that
    enforces Qform/Sform transformations.}
%  \item{onefile}{(logical) allows the scanning of single-file format
%    (.nii) or dual-file format (.hdr and .img) NIfTI files (default =
%    \code{TRUE}).} 
%  \item{gzipped}{(logical) allows the importation of compressed (.gz)
%    files (default = \code{TRUE}).}
  \item{call}{keeps track of the current function call for use in the
    NIfTI extension.}
%  \item{ignoreQform}{is a logical variable (default = \code{FALSE}) that
%    ignores any \sQuote{qform} infomration in the NIfTI header.}
%  \item{ignoreSform}{is a logical variable (default = \code{FALSE}) that
%    ignores any \sQuote{sform} infomration in the NIfTI header.}
}
\details{
  The \code{readNIfTI} function utilizes internal methods \code{readBin}
  and \code{readChar} to efficiently extract information from the binary
  file(s).

  Current acceptable data types include
  \describe{
    \item{\dQuote{UINT8}}{BINARY (1 bit per voxel)}
    \item{\dQuote{INT16}}{SIGNED SHORT (16 bits per voxel)}
    \item{\dQuote{INT32}}{SINGED INT (32 bits per voxel)}
    \item{\dQuote{FLOAT32}}{FLOAT (32 bits per voxel)}
    \item{\dQuote{DOUBLE64}}{DOUBLE (64 bits per voxel)}
    \item{\dQuote{UINT16}}{UNSIGNED SHORT (16 bits per voxel)}
    \item{\dQuote{UINT32}}{UNSIGNED INT (32 bits per voxel)}
    }
}
\value{
  An object of class \code{nifti}.
}
\references{
  NIfTI-1\cr
  \url{http://nifti.nimh.nih.gov/}
}
\seealso{
  \code{\link{readAFNI}}, \code{\link{readANALYZE}}
}
\examples{
\dontrun{
url <- "http://nifti.nimh.nih.gov/nifti-1/data/filtered_func_data.nii.gz"
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "filtered_func_data")
download.file(url, urlfile, quiet=TRUE)
}
## The NIfTI file provided here contains the first 18 volumes (10\%)
## of the original data set
urlfile <- file.path(system.file("nifti", package="oro.nifti"),
                     "filtered_func_data")
(ffd <- readNIfTI(urlfile))
image(ffd, oma=rep(2,4))
orthographic(ffd, oma=rep(2,4))
\dontrun{
## 27 scans of Colin Holmes (MNI) brain co-registered and averaged
## NIfTI two-file format
URL <- "http://imaging.mrc-cbu.cam.ac.uk/downloads/Colin/colin_1mm.tgz"
urlfile <- file.path(tempdir(), "colin_1mm.tgz")
download.file(URL, dest=urlfile, quiet=TRUE)
untar(urlfile, exdir=tempdir())
colin <- readNIfTI(file.path(tempdir(), "colin_1mm"))
image(colin, oma=rep(2,4))
orthographic(colin, oma=rep(2,4))
}
}
\author{
  Brandon Whitcher \email{bwhitcher@gmail.com},\cr
  Volker Schmid \email{volkerschmid@users.sourceforge.net},\cr
  Andrew Thornton \email{zeripath@users.sourceforge.net}
}
\keyword{file}
