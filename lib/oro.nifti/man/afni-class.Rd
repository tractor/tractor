% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{afni-class}
\Rdversion{1.1}
\docType{class}
\alias{afni-class}
\alias{show,afni-method}

\title{Class "afni"}
\description{The AFNI class for medical imaging data.}
\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("afni", data, dim, dimnames, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"array"} contains the
      imaging data}
    \item{\code{DATASET_RANK}:}{Object of class \code{"integer"}}
    \item{\code{DATASET_DIMENSIONS}:}{Object of class \code{"integer"}}
    \item{\code{TYPESTRING}:}{Object of class \code{"character"}}
    \item{\code{SCENE_DATA}:}{Object of class \code{"integer"}}
    \item{\code{ORIENT_SPECIFIC}:}{Object of class \code{"integer"}}
    \item{\code{ORIGIN}:}{Object of class \code{"numeric"}}
    \item{\code{DELTA}:}{Object of class \code{"numeric"}}
    \item{\code{TAXIS_NUMS}:}{Object of class \code{"integer"}}
    \item{\code{TAXIS_FLOATS}:}{Object of class \code{"numeric"}}
    \item{\code{TAXIS_OFFSETS}:}{Object of class \code{"numeric"}}
    \item{\code{IDCODE_STRING}:}{Object of class \code{"character"}}
    \item{\code{IDCODE_DATE}:}{Object of class \code{"character"}}
    \item{\code{BYTEORDER_STRING}:}{Object of class \code{"character"}}
    \item{\code{BRICK_STATS}:}{Object of class \code{"numeric"}}
    \item{\code{BRICK_TYPES}:}{Object of class \code{"integer"}}
    \item{\code{BRICK_FLOAT_FACS}:}{Object of class \code{"numeric"}}
    \item{\code{BRICK_LABS}:}{Object of class \code{"character"}}
    \item{\code{BRICK_STATAUX}:}{Object of class \code{"numeric"}}
    \item{\code{STAT_AUX}:}{Object of class \code{"numeric"}}
    \item{\code{HISTORY_NOTE}:}{Object of class \code{"character"}}
    \item{\code{NOTES_COUNT}:}{Object of class \code{"integer"}}
    \item{\code{NOTE_NUMBER}:}{Object of class \code{"character"}}
    \item{\code{TAGALIGN_MATVEC}:}{Object of class \code{"numeric"}}
    \item{\code{VOLREG_MATVEC}:}{Object of class \code{"array"}}
    \item{\code{VOLREG_ROTCOM}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_CENTER_OLD}:}{Object of class \code{"numeric"}}
    \item{\code{VOLREG_CENTER_BASE}:}{Object of class \code{"numeric"}}
    \item{\code{VOLREG_ROTPARENT_IDCODE}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_ROTPARENT_NAME}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_GRIDPARENT_IDCODE}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_GRIDPARENT_NAME}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_INPUT_IDCODE}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_INPUT_NAME}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_BASE_IDCODE}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_BASE_NAME}:}{Object of class \code{"character"}}
    \item{\code{VOLREG_ROTCOM_NUM}:}{Object of class \code{"integer"}}
    \item{\code{IDCODE_ANAT_PARENT}:}{Object of class \code{"character"}}
    \item{\code{TO3D_ZPAD}:}{Object of class \code{"integer"}}
    \item{\code{IDCODE_WARP_PARENT}:}{Object of class \code{"character"}}
    \item{\code{WARP_TYPE}:}{Object of class \code{"integer"}}
    \item{\code{WARP_DATA}:}{Object of class \code{"numeric"}}
    \item{\code{MARKS_XYZ}:}{Object of class \code{"numeric"}}
    \item{\code{MARKS_LAB}:}{Object of class \code{"character"}}
    \item{\code{MARKS_HELP}:}{Object of class \code{"character"}}
    \item{\code{MARKS_FLAGS}:}{Object of class \code{"integer"}}
    \item{\code{TAGSET_NUM}:}{Object of class \code{"integer"}}
    \item{\code{TAGSET_FLOATS}:}{Object of class \code{"numeric"}}
    \item{\code{TAGSET_LABELS}:}{Object of class \code{"character"}}
    \item{\code{LABEL_1}:}{Object of class \code{"character"}}
    \item{\code{LABEL_2}:}{Object of class \code{"character"}}
    \item{\code{DATASET_NAME}:}{Object of class \code{"character"}}
    \item{\code{DATASET_KEYWORDS}:}{Object of class \code{"character"}}
    \item{\code{BRICK_KEYWORDS}:}{Object of class \code{"character"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{array}"}, from data part.\cr
Class \code{"\linkS4class{matrix}"}, by class "array", distance 2, with
explicit test and coerce.\cr
Class \code{"\linkS4class{structure}"}, by class "array", distance 2.\cr
Class \code{"\linkS4class{vector}"}, by class "array", distance 3, with
explicit coerce.\cr
Class \code{"\linkS4class{vector}"}, by class "array", distance 5, with
explicit test and coerce.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "afni")}: ... }
    \item{writeAFNI}{\code{signature(nim = "afni")}: ... }
	 }
}
\references{
  AFNI\cr
  \url{http://afni.nimh.nih.gov/pub/dist/src/README.attributes}
}
\author{ Karsten Tabelow \email{karsten.tabelow@wias-berlin.de} }
\seealso{
  \code{\linkS4class{nifti}}, \code{\linkS4class{anlz}}
}
\examples{
showClass("afni")
}
\keyword{classes}
