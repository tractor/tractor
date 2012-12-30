% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{nifti-class}
\Rdversion{1.1}
\docType{class}
\alias{nifti-class}
\alias{show,nifti-method}

\title{Class "nifti"}
\description{The NIfTI class for medical imaging data.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("nifti", data,
  dim, dimnames, ...)} or by calling the \code{nifti} function.
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
    \item{\code{dim_info}:}{Object of class \code{"numeric"} contains
      MRI slice ordering}
    \item{\code{dim_}:}{Object of class \code{"vector"} contains the
      dimensions of the imaging data}
    \item{\code{intent_p1}:}{Object of class \code{"numeric"}}
    \item{\code{intent_p2}:}{Object of class \code{"numeric"}}
    \item{\code{intent_p3}:}{Object of class \code{"numeric"}}
    \item{\code{intent_code}:}{Object of class \code{"numeric"}}
    \item{\code{datatype}:}{Object of class \code{"numeric"}}
    \item{\code{bitpix}:}{Object of class \code{"numeric"} contains the
      number of bits per voxel (pixel)}
    \item{\code{slice_start}:}{Object of class \code{"numeric"}}
    \item{\code{pixdim}:}{Object of class \code{"vector"} contains the
      real-world dimensions of the imaging data}
    \item{\code{vox_offset}:}{Object of class \code{"numeric"} contains
      the voxel offset (= 352 when no extensions exist)}
    \item{\code{scl_slope}:}{Object of class \code{"numeric"}}
    \item{\code{scl_inter}:}{Object of class \code{"numeric"}}
    \item{\code{slice_end}:}{Object of class \code{"numeric"}}
    \item{\code{slice_code}:}{Object of class \code{"numeric"}}
    \item{\code{xyzt_units}:}{Object of class \code{"numeric"}}
    \item{\code{cal_max}:}{Object of class \code{"numeric"} contains the
      maximum display intensity}
    \item{\code{cal_min}:}{Object of class \code{"numeric"} contains the
      minimum display intensity}
    \item{\code{slice_duration}:}{Object of class \code{"numeric"}}
    \item{\code{toffset}:}{Object of class \code{"numeric"}}
    \item{\code{glmax}:}{Object of class \code{"numeric"}}
    \item{\code{glmin}:}{Object of class \code{"numeric"}}
    \item{\code{descrip}:}{Object of class \code{"character"}}
    \item{\code{aux_file}:}{Object of class \code{"character"}}
    \item{\code{qform_code}:}{Object of class \code{"numeric"}}
    \item{\code{sform_code}:}{Object of class \code{"numeric"}}
    \item{\code{quatern_b}:}{Object of class \code{"numeric"}}
    \item{\code{quatern_c}:}{Object of class \code{"numeric"}}
    \item{\code{quatern_d}:}{Object of class \code{"numeric"}}
    \item{\code{qoffset_x}:}{Object of class \code{"numeric"}}
    \item{\code{qoffset_y}:}{Object of class \code{"numeric"}}
    \item{\code{qoffset_z}:}{Object of class \code{"numeric"}}
    \item{\code{srow_x}:}{Object of class \code{"vector"}}
    \item{\code{srow_y}:}{Object of class \code{"vector"}}
    \item{\code{srow_z}:}{Object of class \code{"vector"}}
    \item{\code{intent_name}:}{Object of class \code{"character"}}
    \item{\code{magic}:}{Object of class \code{"character"}}
    \item{\code{extender}:}{Object of class \code{"vector"}}
    \item{\code{reoriented}:}{Object of class \code{"logical"}}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{array}"}, from data part.\cr
  Class \code{"\linkS4class{matrix}"}, by class "array", distance 2, with explicit test and coerce.\cr
  Class \code{"\linkS4class{structure}"}, by class "array", distance 2.\cr
  Class \code{"\linkS4class{vector}"}, by class "array", distance 3, with explicit coerce.\cr
  Class \code{"\linkS4class{vector}"}, by class "array", distance 5, with explicit test and coerce.
}
\section{Methods}{
  \describe{
    \item{aux.file<-}{\code{signature(x = "nifti")}: replaces the
      \dQuote{auxiliary file} field} 
    \item{aux.file}{\code{signature(object = "nifti")}: returns the
      \dQuote{auxiliary file} field}
    \item{descrip<-}{\code{signature(x = "nifti")}: replaces the
      \dQuote{description} field}
    \item{descrip}{\code{signature(object = "nifti")}: returns the
      \dQuote{description} field}
    \item{image}{\code{signature(x = "nifti")}: diplays the image(s)}
    \item{orthographic}{\code{signature(x = "nifti")}: displays the image(s)}
    \item{overlay}{\code{signature(x = "nifti", y = "nifti")}: displays
      the image(s)}
    \item{show}{\code{signature(object = "nifti")}: prints out a summary
      of the imaging data}
  }
}
\references{
  NIfTI-1\cr
  \url{http://nifti.nimh.nih.gov/}
}
\author{Brandon Whitcher \email{bwhitcher@gmail.com}}
\seealso{
  \code{\linkS4class{anlz}}, \code{\linkS4class{niftiExtension}},
  \code{\linkS4class{niftiAuditTrail}}
}
\examples{
showClass("nifti")
}
\keyword{classes}
