\name{newDicomMetadataFromFile}
\alias{newDicomMetadataFromFile}
\title{
Read a DICOM file into a DicomMetadata object
}
\description{
  This function reads a DICOM file into a \code{\link{DicomMetadata}} object. Only DICOM files from magnetic resonance scanners are supported.
}
\usage{
newDicomMetadataFromFile(fileName, checkFormat = TRUE, dictionary = NULL)
}
\arguments{
  \item{fileName}{The name of a DICOM file.}
  \item{checkFormat}{If \code{TRUE}, the function will check for the magic string \code{"DICM"} at byte offset 128. This string should be present, but in reality not all files contain it.}
  \item{dictionary}{A tag dictionary to use when reading the file. If \code{NULL} then the built-in \code{\link{dictionary}} will be loaded and used.}
}
\value{
  \code{newDicomMetadataFromFile} returns a DicomMetadata object, or \code{NULL} on failure.
}
\author{Jon Clayden}
\seealso{The DICOM standard, found online at \url{http://dicom.nema.org/}. (Warning: may produce headaches!) Also \code{\link{dictionary}}, and \code{\link{newMriImageFromDicom}} for information on how to create \code{\link{MriImage}} objects from DICOM files.}
