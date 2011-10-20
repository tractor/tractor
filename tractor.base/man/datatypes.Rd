\encoding{UTF-8}
\name{data types}
\alias{getDataTypeByNiftiCode}
\title{MriImage data types}
\description{
  Convert a numeric type code into the image data type specifier used in \code{MriImage} objects.
}
\usage{
getDataTypeByNiftiCode(code)
}
\arguments{
  \item{code}{A numeric value indicating the data type code required. Supported values in TractoR are 2 (8 bit unsigned integer), 4 (16 bit signed integer), 8 (32 bit signed integer), 16 (32 bit floating point), 64 (64 bit floating point), 256 (8 bit signed integer), 512 (16 bit unsigned integer) and 768 (32 bit unsigned integer).}
}
\value{
  A list with the following elements
  \item{type}{An \R storage mode, either \code{integer} or \code{double}.}
  \item{size}{The number of bytes per voxel in this data type.}
  \item{isSigned}{Logical value indicating whether data values are signed.}
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{\code{\link{MriImage}}, and the NIfTI-1 standard (\url{http://nifti.nimh.nih.gov/nifti-1}).}
