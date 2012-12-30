% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{translateCoordinate}
\alias{translateCoordinate}
\title{translateCoordinate}
\description{
  Translates a voxel index into the continuous coordinate space defined
  by the NIfTI qform and sform information.
}
\usage{
translateCoordinate(i, nim, verbose=FALSE)
}
\arguments{
  \item{i}{An index vector in \code{nim}.}
  \item{nim}{An object of class \code{nifti}.}
  \item{verbose}{Provide detailed output to the user.}
}
\details{
  This function takes as input a \code{nifti} object and an index vector
  in the voxel space of the object and translates that voxel index into
  the continuous coordinate space defined by the object's qform and
  sform.

  Please note:
  \enumerate{
    \item By default the index \code{i} varies most rapidly, etc.
    \item The ANALYZE 7.5 coordinate system is
    \tabular{ccl}{
      +x \tab = \tab Left\cr
      +y \tab = \tab Anterior\cr
      +z \tab = \tab Superior
    }
    (A left-handed co-ordinate system)
    \item The three methods below give the locations of the voxel centres
    in the x,y,z system.  In many cases programs will want to display the
    data on other grids.  In which case the program will be required to
    convert the desired (x,y,z) values in to voxel values using the
    inverse transformation.
    \item Method 2 uses a factor \code{qfac} which is either -1 or 1.
    \code{qfac} is stored in \code{pixdim[0]}.  If \code{pixdim[0]} != 1
    or -1, which should not occur, we assume 1.
    \item The units of the \code{xyzt} are set in \code{xyzt_units} field.
  }
}
\value{
  A \code{nifti}-class object with translated coordinates.
}
%\references{}
%\seealso{}
\examples{
ffd <- readNIfTI(file.path(system.file("nifti", package="oro.nifti"),
                           "filtered_func_data"))
xyz <- c(1,1,1)
translateCoordinate(xyz, ffd, verbose=TRUE)
xyz <- trunc(dim(ffd)[1:3]/2)
translateCoordinate(xyz, ffd, verbose=TRUE)
}
\author{Andrew Thornton \email{zeripath@users.sourceforge.net}}
%\keyword{}
