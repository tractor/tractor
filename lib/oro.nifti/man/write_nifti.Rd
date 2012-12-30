% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{writeNIfTI-methods}
\docType{methods}
\alias{writeNIfTI-methods}
\alias{writeNIfTI,nifti-method}
\alias{writeNIfTI,anlz-method}
\alias{writeNIfTI,array-method}
\alias{writeNIfTI}
\title{writeNIfTI}
\description{
  This function saves a NIfTI-class object to a single binary file in
  NIfTI format.
}
\usage{
\S4method{writeNIfTI}{nifti}(nim, filename, onefile = TRUE, gzipped = TRUE,
            verbose = FALSE, warn = -1)
\S4method{writeNIfTI}{anlz}(nim, filename, onefile = TRUE, gzipped = TRUE,
            verbose = FALSE, warn = -1)
\S4method{writeNIfTI}{array}(nim, filename, onefile = TRUE, gzipped = TRUE,
            verbose = FALSE, warn = -1)
}
\arguments{
  \item{nim}{is an object of class \code{nifti} or \code{anlz}.}
  \item{filename}{is the path and file name to save the NIfTI file
    (.nii) \bold{without} the suffix.}
  \item{onefile}{is a logical value that allows the scanning of
    single-file (.nii) or dual-file format (.hdr and .img) NIfTI files
    (default = \code{TRUE}).} 
  \item{gzipped}{is a character string that enables exportation of
    compressed (.gz) files (default = \code{TRUE}).}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{\link{options}} for more details.}
}
\section{Methods}{
  \describe{
  \item{object = "anlz"}{Convert ANALYZE object to class \code{nifti}
    and write the NIfTI volume to disk.}
  \item{object = "array"}{Convert array to class \code{nifti} and write
    the NIfTI volume to disk.}
  \item{object = "nifti"}{Write NIfTI volume to disk.}
  }
}
\details{
  The \code{writeNIfTI} function utilizes the internal \code{writeBin}
  and \code{writeChar} command to write information to a binary file.

  Current acceptable data types include
  \describe{
    \item{\dQuote{UINT8}}{DT BINARY (1 bit per voxel)}
    \item{\dQuote{INT16}}{DT SIGNED SHORT (16 bits per voxel)}
    \item{\dQuote{INT32}}{DT SINGED INT (32 bits per voxel)}
    \item{\dQuote{FLOAT32}}{DT FLOAT (32 bits per voxel)}
    \item{\dQuote{DOUBLE64}}{DT DOUBLE (64 bits per voxel)}
    \item{\dQuote{UINT16}}{DT UNSIGNED SHORT (16 bits per voxel)}
    }
}
\value{
  Nothing.
}
\references{
  NIfTI-1\cr
  \url{http://nifti.nimh.nih.gov/}
}
\seealso{
  \code{\link{writeAFNI}}, \code{\link{writeANALYZE}}
}
\examples{
norm <- dnorm(seq(-5, 5, length=32), sd=2)
norm <- (norm-min(norm)) / max(norm-min(norm))
img <- outer(outer(norm, norm), norm)
img <- round(255 * img)
img[17:32,,] <- 255 - img[17:32,,]
img.nifti <- nifti(img) # create NIfTI object

writeNIfTI(img.nifti, "test-nifti-image-uint8", verbose=TRUE)
## These files should be viewable in, for example, FSLview
## Make sure you adjust the min/max values for proper visualization
data <- readNIfTI("test-nifti-image-uint8", verbose=TRUE)
image(img.nifti, oma=rep(2,4), bg="white")
image(data, oma=rep(2,4), bg="white")
abs.err <- abs(data - img.nifti)
image(as(abs.err, "nifti"), zlim=range(img.nifti), oma=rep(2,4),
      bg="white")

\dontrun{
## Loop through all possible data types
datatypes <- list(code=c(2, 4, 8, 16, 64),
                  name=c("uint8", "int16", "int32", "float", "double"))
equal <- vector("list")
for (i in 1:length(datatypes$code)) {
  fname <- paste("test-nifti-image-", datatypes$name[i], sep="")
  rm(img.nifti)
  img.nifti <- nifti(img, datatype=datatypes$code[i])
  writeNIfTI(img.nifti, fname, verbose=TRUE)
  equal[[i]] <- all(readNIfTI(fname) == img)
}
names(equal) <- datatypes$name
unlist(equal)
}
}
\author{
  Brandon Whitcher \email{bwhitcher@gmail.com},\cr
  Volker Schmid \email{volkerschmid@users.sourceforge.net}
}
\keyword{file}
\keyword{methods}
