\name{getDataTypeByNiftiCode}
\alias{getDataTypeByNiftiCode}
\title{MriImage data types}
\description{
  Convert a numeric type code into the image data type specifier used in \code{MriImage} objects.
}
\usage{
getDataTypeByNiftiCode(code)
}
\arguments{
  \item{code}{A numeric value indicating the data type code required. Supported values in TractoR are 2, 4, 8, 16, 64, 256, 512 and 768.}
}
\value{
  A list with the following elements
  \item{type}{An \R storage mode, either \code{integer} or \code{double}.}
  \item{size}{The number of bytes per voxel in this data type.}
  \item{isSigned}{Logical value indicating whether data values are signed.}
}
\author{Jon Clayden}
\seealso{\code{\link{MriImage}}, and the NIfTI-1 standard (\url{http://nifti.nimh.nih.gov/nifti-1}).}
