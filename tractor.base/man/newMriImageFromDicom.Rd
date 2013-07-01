\name{newMriImageFromDicomDirectory}
\alias{readDicomDirectory}
\alias{newMriImageFromDicomDirectory}
\title{Functions for reading images from DICOM files}
\description{
  Functions for reading images from a directory of DICOM files.
}
\usage{
readDicomDirectory(dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
newMriImageFromDicomDirectory(dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
}
\arguments{
  \item{dicomDir}{Character vector of length one giving the name of a directory containing DICOM files.}
  \item{readDiffusionParams}{Logical value: should diffusion MRI parameters (b-values and gradient directions) be retrieved from the files if possible?}
  \item{untileMosaics}{Logical value: should Siemens mosaic images be converted into 3D volumes? This may occasionally be performed in error, which can be prevented by setting this value to \code{FALSE}.}
}
\note{
  These two functions are equivalent, but \code{readDicomDirectory} is preferred for brevity. The \code{newMriImageFromDicomDirectory} function is likely to be deprecated in a future release.
}
\value{
  A list containing elements
    \item{image}{An \code{\linkS4class{MriImage}} object.}
    \item{bValues}{Diffusion b-values, if requested. Will be \code{NA} if the information could not be found in the files.}
    \item{bVectors}{Diffusion gradient vectors, if requested. Will be \code{NA} if the information could not be found in the files.}
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{\code{\linkS4class{DicomMetadata}}, \code{\linkS4class{MriImage}}, \code{\link{sortDicomDirectory}}.}