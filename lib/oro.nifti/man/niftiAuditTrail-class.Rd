% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{niftiAuditTrail-class}
\Rdversion{1.1}
\docType{class}
\alias{niftiAuditTrail-class}

\title{Class "niftiAuditTrail"}
\description{
  An extension of the NIfTI class that adds an audit trail in XML format. 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("niftiAuditTrail", data, dim, dimnames, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"array"}}
    \item{\code{trail}:}{Object of class \code{"XMLAbstractNode"}
      contains the XML version of the audit trail.}
    \item{\code{extensions}:}{Object of class \code{"list"} contains the
      list of all extensions.}
    \item{\code{sizeof_hdr}:}{Object of class \code{"numeric"}}
    \item{\code{data_type}:}{Object of class \code{"character"}}
    \item{\code{db_name}:}{Object of class \code{"character"}}
    \item{\code{extents}:}{Object of class \code{"numeric"}}
    \item{\code{session_error}:}{Object of class \code{"numeric"}}
    \item{\code{regular}:}{Object of class \code{"character"}}
    \item{\code{dim_info}:}{Object of class \code{"numeric"}}
    \item{\code{dim_}:}{Object of class \code{"vector"}}
    \item{\code{intent_p1}:}{Object of class \code{"numeric"}}
    \item{\code{intent_p2}:}{Object of class \code{"numeric"}}
    \item{\code{intent_p3}:}{Object of class \code{"numeric"}}
    \item{\code{intent_code}:}{Object of class \code{"numeric"}}
    \item{\code{datatype}:}{Object of class \code{"numeric"}}
    \item{\code{bitpix}:}{Object of class \code{"numeric"}}
    \item{\code{slice_start}:}{Object of class \code{"numeric"}}
    \item{\code{pixdim}:}{Object of class \code{"vector"}}
    \item{\code{vox_offset}:}{Object of class \code{"numeric"}}
    \item{\code{scl_slope}:}{Object of class \code{"numeric"}}
    \item{\code{scl_inter}:}{Object of class \code{"numeric"}}
    \item{\code{slice_end}:}{Object of class \code{"numeric"}}
    \item{\code{slice_code}:}{Object of class \code{"numeric"}}
    \item{\code{xyzt_units}:}{Object of class \code{"numeric"}}
    \item{\code{cal_max}:}{Object of class \code{"numeric"}}
    \item{\code{cal_min}:}{Object of class \code{"numeric"}}
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
  Class \code{"\linkS4class{niftiExtension}"}, directly.\cr
  Class \code{"\linkS4class{nifti}"}, by class "niftiExtension", distance 2.\cr
  Class \code{"\linkS4class{array}"}, by class "niftiExtension", distance 3.\cr
  Class \code{"\linkS4class{matrix}"}, by class "niftiExtension", distance 4, with explicit test and coerce.\cr
  Class \code{"\linkS4class{structure}"}, by class "niftiExtension", distance 4.\cr
  Class \code{"\linkS4class{vector}"}, by class "niftiExtension", distance 5, with explicit coerce.\cr
  Class \code{"\linkS4class{vector}"}, by class "niftiExtension", distance 7, with explicit test and coerce.
}
\section{Methods}{
  No methods defined with class "niftiAuditTrail" in the signature.
}
\references{
  NIfTI-1\cr
  \url{http://nifti.nimh.nih.gov/}
}
\author{Andrew Thornton \email{zeripath@users.sourceforge.net}}
\seealso{
  \code{\linkS4class{nifti}}, \code{\linkS4class{niftiExtension}}
}
\examples{
showClass("niftiAuditTrail")
}
\keyword{classes}
