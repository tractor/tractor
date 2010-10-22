\name{newMriImageFromDicom}
\alias{newMriImageFromDicom}
\alias{newMriImageFromDicomDirectory}
\alias{newMriImageFromDicomMetadata}
\alias{newMriImageMetadataFromDicom}
\alias{newMriImageMetadataFromDicomMetadata}
\title{Functions for reading images from DICOM files}
\description{
  Functions for reading images from DICOM files.
}
\usage{
newMriImageFromDicom(fileName)
newMriImageFromDicomDirectory(dicomDir, readDiffusionParams = FALSE)
newMriImageFromDicomMetadata(metadata, flipY = TRUE)
newMriImageMetadataFromDicom(fileName)
newMriImageMetadataFromDicomMetadata(dicom)
}
\arguments{
  \item{fileName}{Character vector of length one giving the name of a DICOM file.}
  \item{dicomDir}{Character vector of length one giving the name of a directory containing DICOM files.}
  \item{readDiffusionParams}{Logical value: should diffusion MRI parameters (b-values and gradient directions) be retrieved from the files if possible?}
  \item{metadata, dicom}{DicomMetadata objects.}
  \item{flipY}{Logical value: should the image be flipped in the Y direction? Usually this is appropriate to convert between DICOM's LPS and TractoR's LAS storage conventions.}
}
\value{
  \code{newMriImageFromDicom} and \code{newMriImageFromDicomMetadata} return an MriImage object. \code{newMriImageMetadataFromDicom} and \code{newMriImageMetadataFromDicomMetadata} return an MriImageMetadata object. \code{newMriImageFromDicomDirectory} returns a list containing elements
  \item{image}{An MriImage object.}
  \item{bValues}{Diffusion b-values, if requested. Will be NA if the information could not be found in the files.}
  \item{bVectors}{Diffusion gradient vectors, if requested. Will be NA if the information could not be found in the files.}
}
\author{Jon Clayden}
\seealso{\code{\link{DicomMetadata}}, \code{\link{MriImage}}, \code{\link{MriImageMetadata}}, \code{\link{sortDicomDirectory}}.}