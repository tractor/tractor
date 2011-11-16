\name{newMriImageMetadataFromTemplate}
\alias{newMriImageMetadataFromTemplate}
\title{Duplicates an MriImageMetadata object, possibly with modification}
\description{
Duplicates an MriImageMetadata object, possibly with modification.
}
\usage{
newMriImageMetadataFromTemplate(metadata, imageDims = NA, voxelDims = NA, voxelUnit = NA,
                                  datatype = NA, origin = NA, tags = NA)
}
\arguments{
  \item{metadata}{An existing \code{\link{MriImageMetadata}} object.}
  \item{imageDims}{Image dimensions.}
  \item{voxelDims}{Voxel dimensions.}
  \item{voxelUnit}{Voxel dimension unit, usually \code{"mm"}.}
  \item{datatype}{Image data type. See \code{\link{getDataTypeByNiftiCode}}.}
  \item{origin}{Coordinate origin.}
  \item{tags}{A list containing elements \code{keys} and \code{values}, specifying tags associated with the image.}
}
\value{
  An \code{MriImageMetadata} object, like the one specified in the first argument, but with the specified fields replaced by new values. Any field with value \code{NA} (the default except for \code{source}) will be taken from the original metadata object.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{\code{\linkS4class{MriImageMetadata}}, \code{\linkS4class{MriImage}}, \code{\link{getDataTypeByNiftiCode}}.}
