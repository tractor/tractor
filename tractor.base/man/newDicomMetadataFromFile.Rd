\name{newDicomMetadataFromFile}
\alias{newDicomMetadataFromFile}
\title{
Read a DICOM file into a DicomMetadata object
}
\description{
  This function reads a DICOM file into a \code{\link{DicomMetadata}} object. Only DICOM files from magnetic resonance scanners are supported.
}
\usage{
newDicomMetadataFromFile(fileName, checkFormat = TRUE, dictionary = NULL,
    stopTag = NULL, ignoreTransferSyntax = FALSE)
}
\arguments{
  \item{fileName}{The name of a DICOM file.}
  \item{checkFormat}{If \code{TRUE}, the function will check for the magic string \code{"DICM"} at byte offset 128. This string should be present, but in reality not all files contain it.}
  \item{dictionary}{A tag dictionary to use when reading the file. If \code{NULL} then the built-in \code{\link{dictionary}} will be loaded and used.}
  \item{stopTag}{An integer vector giving the group and element numbers (in that order) of a DICOM tag, or \code{NULL}. If not \code{NULL}, the function will stop parsing the DICOM file if the specified tag is encountered. This can be used to speed up the process if a specific tag is required.}
  \item{ignoreTransferSyntax}{If \code{TRUE}, any transfer syntax stored in the file will be ignored, and the code will try to deduce the transfer syntax using heuristics. This may occasionally be necessary for awkward DICOM files, but is not generally recommended.}
}
\value{
  \code{newDicomMetadataFromFile} returns a \code{\linkS4class{DicomMetadata}} object, or \code{NULL} on failure.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{The DICOM standard, found online at \url{http://dicom.nema.org/}. (Warning: may produce headaches!) Also \code{\link{dictionary}}, and \code{\link{newMriImageFromDicomDirectory}} for information on how to create \code{\linkS4class{MriImage}} objects from DICOM files.}
