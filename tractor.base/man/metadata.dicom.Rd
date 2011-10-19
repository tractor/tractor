\name{DicomMetadata}
\alias{DicomMetadata}
\alias{isDicomMetadata}
\alias{metadata.dicom}
\alias{print.metadata.dicom}
\alias{newDicomMetadataFromFile}
\title{Class representing metadata extracted from a DICOM header}
\description{
  This class represents DICOM metadata, which typically contains detailed information about the scan parameters and subject. Only DICOM files from magnetic resonance scanners are supported.
}
\usage{
newDicomMetadataFromFile(fileName, checkFormat = TRUE, dictionary = NULL)
isDicomMetadata(object)
\S3method{print}{metadata.dicom}(x, descriptions = FALSE, ...)
}
\arguments{
  \item{fileName}{The name of a DICOM file.}
  \item{checkFormat}{If TRUE, the function will check for the magic string "DICM" at byte offset 128. This string should be present, but in reality not all files contain it.}
  \item{dictionary}{A tag dictionary to use when reading the file. If NULL then the built-in dictionary will be loaded and used.}
  \item{object}{Any object.}
  \item{x}{Object of class \code{metadata.dicom}.}
  \item{descriptions}{Logical value: should tag descriptions be given? Numeric tag numbers are printed if this is FALSE (the default).}
  \item{...}{Further options to \code{print} (though none are supported for this type of object).}
}
\details{
  Tag descriptions and types are read from the DICOM \code{\link{dictionary}}.
}
\value{
  \code{newDicomMetadataFromFile} returns a DicomMetadata object, or NULL on failure. \code{isDicomMetadata} returns TRUE if \code{object} is a DicomMetadata object. \code{print.metadata.dicom} is only called for its side-effect.
  
  A DicomMetadata object is a list object (see \code{\link{list.object}}) with class \code{c("metadata.dicom", "list.object", "list")} and the following function elements.
  \item{getAvailableTags()}{Retrieve a list of lists representing all available tags. Each element of the outer list has identifying elements "group" and "element". See the DICOM standard for more information.}
  \item{getDataLength()}{Retrieve the length of the data part of the DICOM file in bytes.}
  \item{getDataOffset()}{Retrieve the byte offset of the data in the DICOM file.}
  \item{getEndianness()}{Retrieve the endianness of the file: "big" or "little".}
  \item{getSource()}{Retrieve the name of the source file.}
  \item{getTagOffset()}{Retrieve the byte offset of useful tags in the file.}
  \item{getTagValue(group, element)}{Retrieve the value of the tag with specified group and element numbers, usually given in hex.}
  \item{nTags()}{Retrieve the number of tags stored in this object.}
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{The DICOM standard, found online at \url{http://dicom.nema.org/}. (Warning: may produce headaches!) Also \code{\link{dictionary}}, and \code{\link{newMriImageFromDicom}} for information on how to create MriImage objects from DICOM files.}
