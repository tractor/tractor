\name{DicomMetadata-class}
\Rdversion{1.1}
\docType{class}
\alias{DicomMetadata}
\alias{DicomMetadata-class}

\title{Class \code{"DicomMetadata"}}
\description{
This class represents DICOM metadata, which typically contains detailed information about the scan parameters and subject.
}
\section{Extends}{
Class \code{"\linkS4class{SerialisableObject}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\author{Jon Clayden}
\examples{
showClass("DicomMetadata")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{source}:}{Object of class \code{character}.}
    \item{\code{tags}:}{Object of class \code{data.frame}.}
    \item{\code{tagOffset}:}{Object of class \code{integer}.}
    \item{\code{dataOffset}:}{Object of class \code{integer}.}
    \item{\code{dataLength}:}{Object of class \code{integer}.}
    \item{\code{explicitTypes}:}{Object of class \code{logical}.}
    \item{\code{endian}:}{Object of class \code{character}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{getTags()}:}{Retrieve the data frame containing all available tags, which has columns \code{"groups"}, \code{"elements"}, \code{"types"} and \code{"values"}. See the DICOM standard for more information.}
    \item{\code{getTagValue(group, element)}:}{Retrieve the value of the tag with specified group and element numbers, usually given in hex.}
    \item{\code{getTagOffset()}:}{Retrieve the byte offset of useful tags in the file.}
    \item{\code{getDataOffset()}:}{Retrieve the byte offset of the data in the DICOM file.}
    \item{\code{getDataLength()}:}{Retrieve the length of the data part of the DICOM file in bytes.}
    \item{\code{getSource()}:}{Retrieve the name of the source file.}
    \item{\code{getEndianness()}:}{Retrieve the endianness of the file: \code{"big"} or \code{"little"}.}
    \item{\code{nTags()}:}{Retrieve the number of tags stored in this object.}
  }

The following methods are inherited (from the corresponding class):
serialise ("SerialisableObject"), methods ("SerialisableObject")
}
