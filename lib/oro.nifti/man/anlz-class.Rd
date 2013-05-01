% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{anlz-class}
\Rdversion{1.1}
\docType{class}
\alias{anlz-class}
\alias{show,anlz-method}

\title{Class "anlz"}
\description{The ANALYZE class for medical imaging data.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("anlz", data,
    dim, dimnames, ...)} or by calling the \code{anlz} function. 
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"array"} contains the
      imaging data}
    \item{\code{sizeof_hdr}:}{Object of class \code{"numeric"} contains
      the size of the header (= 348)}
    \item{\code{data_type}:}{Object of class \code{"character"}}
    \item{\code{db_name}:}{Object of class \code{"character"}}
    \item{\code{extents}:}{Object of class \code{"numeric"}}
    \item{\code{session_error}:}{Object of class \code{"numeric"}}
    \item{\code{regular}:}{Object of class \code{"character"}}
    \item{\code{hkey_un0}:}{Object of class \code{"character"}}
    \item{\code{dim_}:}{Object of class \code{"vector"} contains the
      dimensions of the imaging data}
    \item{\code{vox_units}:}{Object of class \code{"character"}}
    \item{\code{cal_units}:}{Object of class \code{"character"}}
    \item{\code{unused1}:}{Object of class \code{"numeric"}}
    \item{\code{datatype}:}{Object of class \code{"numeric"}}
    \item{\code{bitpix}:}{Object of class \code{"numeric"} contains the
      number of bits per voxel (pixel)}
    \item{\code{dim_un0}:}{Object of class \code{"numeric"}}
    \item{\code{pixdim}:}{Object of class \code{"vector"} contains the
      real-world dimensions of the imaging data}
    \item{\code{vox_offset}:}{Object of class \code{"numeric"}}
    \item{\code{funused1}:}{Object of class \code{"numeric"}}
    \item{\code{funused2}:}{Object of class \code{"numeric"}}
    \item{\code{funused3}:}{Object of class \code{"numeric"}}
    \item{\code{cal_max}:}{Object of class \code{"numeric"} contains the
      maximum display intensity}
    \item{\code{cal_min}:}{Object of class \code{"numeric"} contains the
      minimum display intensity}
    \item{\code{compressed}:}{Object of class \code{"numeric"}}
    \item{\code{verified}:}{Object of class \code{"numeric"}}
    \item{\code{glmax}:}{Object of class \code{"numeric"}}
    \item{\code{glmin}:}{Object of class \code{"numeric"}}
    \item{\code{descrip}:}{Object of class \code{"character"}}
    \item{\code{aux_file}:}{Object of class \code{"character"}}
    \item{\code{orient}:}{Object of class \code{"character"}}
    \item{\code{origin}:}{Object of class \code{"numeric"}}
    \item{\code{generated}:}{Object of class \code{"character"}}
    \item{\code{scannum}:}{Object of class \code{"character"}}
    \item{\code{patient_id}:}{Object of class \code{"character"}}
    \item{\code{exp_date}:}{Object of class \code{"character"}}
    \item{\code{exp_time}:}{Object of class \code{"character"}}
    \item{\code{hist_un0}:}{Object of class \code{"character"}}
    \item{\code{views}:}{Object of class \code{"numeric"}}
    \item{\code{vols_added}:}{Object of class \code{"numeric"}}
    \item{\code{start_field}:}{Object of class \code{"numeric"}}
    \item{\code{field_skip}:}{Object of class \code{"numeric"}}
    \item{\code{omax}:}{Object of class \code{"numeric"}}
    \item{\code{omin}:}{Object of class \code{"numeric"}}
    \item{\code{smax}:}{Object of class \code{"numeric"}}
    \item{\code{smin}:}{Object of class \code{"numeric"}}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{array}"}, from data part.\cr
  Class \code{"\linkS4class{matrix}"}, by class "array", distance 2,
  with explicit test and coerce. \cr
  Class \code{"\linkS4class{structure}"}, by class "array", distance 2.\cr
  Class \code{"\linkS4class{vector}"}, by class "array", distance 3,
  with explicit coerce.\cr
  Class \code{"\linkS4class{vector}"}, by class "array", distance 5,
  with explicit test and coerce.
}
\section{Methods}{
  \describe{
    \item{descrip<-}{\code{signature(x = "anlz")}: replaces the
      \dQuote{description} field}
    \item{descrip}{\code{signature(object = "anlz")}: returns the
      \dQuote{description} field}
    \item{image}{\code{signature(x = "anlz")}: diplays the image(s)}
    \item{show}{\code{signature(object = "anlz")}: prints out a summary
      of the imaging data}
  }
}
\references{ANALYZE 7.5\cr\url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}}
\author{ Brandon Whitcher \email{bwhitcher@gmail.com} }
\seealso{
  \code{\linkS4class{nifti}}, \code{\linkS4class{niftiExtension}}
}
\examples{
showClass("anlz")
}
\keyword{classes}
