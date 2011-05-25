\name{DicomMetadata-class}
\Rdversion{1.1}
\docType{class}
\alias{DicomMetadata-class}
\alias{DicomMetadata}

\title{Class "DicomMetadata"}
\description{
  This class represents DICOM metadata, which typically contains detailed information about the scan parameters and subject.
}
\section{Extends}{
Class \code{"\linkS4class{SerialisableObject}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\section{Methods}{
  \describe{
    \item{\code{getAvailableTags()}:}{Retrieve a list of lists representing all available tags. Each element of the outer list has identifying elements \code{"group"} and \code{"element"}. See the DICOM standard for more information.}
    \item{\code{getDataLength()}:}{Retrieve the length of the data part of the DICOM file in bytes.}
    \item{\code{getDataOffset()}:}{Retrieve the byte offset of the data in the DICOM file.}
    \item{\code{getEndianness()}:}{Retrieve the endianness of the file: \code{"big"} or \code{"little"}.}
    \item{\code{getSource()}:}{Retrieve the name of the source file.}
    \item{\code{getTagOffset()}:}{Retrieve the byte offset of useful tags in the file.}
    \item{\code{getTagValue(group, element)}:}{Retrieve the value of the tag with specified group and element numbers, usually given in hex.}
    \item{\code{nTags()}:}{Retrieve the number of tags stored in this object.}
  }
}
\author{Jon Clayden}
\seealso{\code{\link{newDicomMetadataFromFile}}, which can be used to create a \code{DicomMetadata} object by reading tags from a DICOM file.}
\keyword{classes}
