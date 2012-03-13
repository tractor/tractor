% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{convert.scene}
\alias{convert.scene}
\docType{data}
\title{Convert AFNI data codes}
\description{
  Codes that appear in the AFNI header are mapped to meaningful
  character strings.
}
\usage{convert.scene(scene.data, typestring)}
\arguments{
  \item{scene.data}{defines data type.}
  \item{typestring}{defines whether func or anat data.}
}
\details{
  \code{switch} statements are used to map a numeric code to the
  appropriate string.
}
\value{
  A character string.
}
\references{
  AFNI\cr
  \url{http://afni.nimh.nih.gov/pub/dist/src/README.attributes}
}
\seealso{
  \code{\link{convert.datatype.anlz}}, \code{\link{convert.orient.anlz}}
}
\examples{
## 4 = CT for anatomic data
convert.scene(4, "3DIM_HEAD_ANAT")
}
\author{ Karsten Tabelow \email{karsten.tabelow@wias-berlin.de} }
\keyword{ misc }
