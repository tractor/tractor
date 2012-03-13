% Part of the dcemri package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{Convert ANALYZE Codes}
\alias{convert.bitpix.anlz}
\alias{convert.datatype.anlz}
\alias{convert.orient.anlz}
\title{Convert NIfTI Codes}
\description{
  Codes that appear in the ANALYZE header are mapped to meaningful
  chartacter strings.
}
\usage{
convert.bitpix.anlz(bitpix)
convert.datatype.anlz(datatype.code)
convert.orient.anlz(orientation)
}
\arguments{
  \item{bitpix}{is the bit-per-pixel code.}
  \item{datatype.code}{defines data type.}
  \item{orientation}{defines the orientation.}
}
\details{
  \code{switch} statements are used to map a numeric code to the
  appropriate string.
}
\value{
  A character string.
}
\references{
  ANALYZE 7.5\cr\url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}
}
\seealso{
  \code{\link{convert.datatype}}, \code{\link{convert.bitpix}},
  \code{\link{convert.intent}}, \code{\link{convert.form}},
  \code{\link{convert.units}}, \code{\link{convert.slice}}
}
\examples{
##  4 = SIGNED_SHORT
convert.datatype.anlz(4)
## 16 = FLOAT
convert.datatype.anlz(16)
##  2 = "saggital unflipped"
convert.orient.anlz(2)
##  4 = "coronal flipped"
convert.orient.anlz(4)
}
\author{ Brandon Whitcher \email{bjw34032@users.sourceforge.net} }
%\keyword{}
