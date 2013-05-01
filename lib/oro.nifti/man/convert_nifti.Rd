% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{Convert NIfTI Codes}
\alias{convert.datatype}
\alias{convert.bitpix}
\alias{convert.intent}
\alias{convert.form}
\alias{convert.units}
\alias{convert.slice}
\title{Convert NIfTI Codes}
\description{
  Codes that appear in the NIfTI-1 header are mapped to meaningful
  chartacter strings.
}
\usage{
convert.datatype(datatype.code = NULL)
convert.bitpix(bitpix = NULL)
convert.intent(intent.code = NULL)
convert.form(form.code)
convert.units(units, inverse = FALSE)
convert.slice(slice.code)
}
\arguments{
  \item{datatype.code}{defines data type.}
  \item{bitpix}{is the bit-per-pixel code.}
  \item{intent.code}{is the NIfTI intent code.}
  \item{form.code}{is the \eqn{(x,y,z)} coordinate system.}
  \item{units}{is the units of pixdim[1..4].}
  \item{inverse}{is a logical value that denotes the direction of unit
    conversion.}
  \item{slice.code}{is the slice timing order.}
}
\details{
  \code{switch} statements are used to map a numeric code to the
  appropriate string.
}
\value{
  A character string.
}
\references{
  Neuroimaging Informatics Technology Initiative (NIfTI)\cr
  \url{http://nifti.nimh.nih.gov/}
}
\seealso{
  \code{\link{convert.datatype.anlz}}, \code{\link{convert.orient.anlz}}
}
\examples{
## No arguments produces a list structure
## Use the abbreviation to obtain the code
convert.datatype()$INT16
## Use the code directly to obtain the abbreviation
convert.datatype(4)
## No arguments produces a list structure
## Use the abbreviation to obtain the code
convert.bitpix()$INT16
## No arguments produces a list structure
## Use the abbreviation to obtain the code
convert.intent()$Estimate
## Use the code directly to obtain the abbreviation
convert.intent(1001)
## 4 = MNI_152
convert.form(4)
## 2 = mm
convert.units(2)
## 8 = sec
convert.units(8)
## 0 = Unknown
convert.slice(0)
}
\author{ Brandon Whitcher \email{bwhitcher@gmail.com} }
%\keyword{}
