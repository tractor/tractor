% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{overlay-methods}
\docType{methods}
\alias{overlay}
\alias{overlay-methods}
\alias{overlay,nifti,nifti-method}
\alias{overlay,nifti,anlz-method}
\alias{overlay,nifti,array-method}
\alias{overlay,anlz,nifti-method}
\alias{overlay,anlz,anlz-method}
\alias{overlay,anlz,array-method}
\alias{overlay,array,nifti-method}
\alias{overlay,array,anlz-method}
\alias{overlay,array,array-method}
\alias{overlay,afni,afni-method}
\alias{overlay,afni,array-method}
\alias{overlay.nifti}
\title{Methods for Function overlay}
\description{Methods for function \code{overlay}}
\usage{
\S4method{overlay}{nifti,nifti}(x, y, z=1, w=1, col.x=gray(0:64/64),
        col.y=hotmetal(), zlim.x=NULL, zlim.y=NULL,
        plane=c("axial", "coronal", "sagittal"),
        plot.type=c("multiple","single"), xlab="", ylab="",
        axes=FALSE, oma=rep(0,4), mar=rep(0,4), bg="black", ...)
%\S4method{overlay}{anlz,anlz}(x, y, z=1, w=1, col.x=gray(0:64/64),
%      col.y=hotmetal(), zlim.x=NULL, zlim.y=NULL,
%      plot.type=c("multiple","single"), xlab="", ylab="",
%      axes=FALSE, oma=rep(0,4), mar=rep(0,4), bg="black", ...)
}
\arguments{
  \item{x,y}{is an object of class \code{nifti} or similar.}
  \item{z}{is the slice to be displayed (ignored when \code{plot.type =
      "multiple"}).}
  \item{w}{is the time point to be displayed (4D arrays only).}
  \item{col.x}{is grayscale (by default).}
  \item{col.y}{is hotmetal (by default).}
  \item{zlim.x,zlim.y}{are set to \code{NULL} (by default) and taken
    from the header information.}
  \item{plane}{is the plane of acquisition to be displayed (choices are
    \sQuote{axial}, \sQuote{coronal}, \sQuote{sagittal}).}
  \item{plot.type}{allows the choice between all slices being displayed,
    in a matrix (left-to-right, top-to-bottom), or a single slice.}
  \item{xlab}{is set to \dQuote{} since all margins are set to zero.}
  \item{ylab}{is set to \dQuote{} since all margins are set to zero.}
  \item{axes}{is set to \code{FALSE} since all margins are set to zero.}
  \item{oma}{is the size of the outer margins in the \code{par} function.}
  \item{mar}{is the number of lines of margin in the \code{par} function.}
  \item{bg}{is the background color in the \code{par} function.}
  \item{\dots}{other arguments to the \code{image} function may be
    provided here.}
}
\details{
  The \code{image} command is used multiple times to simultaneously
  visualize one of the three orthogonal planes in two
  multidimensional arrays, one on top of the other, for medical imaging
  data.
}
\section{Methods}{
\describe{
  \item{x = "nifti", y = "nifti"}{Produce overlay of \code{y} on \code{x}.}
  \item{x = "anlz", y = "anlz"}{Produce overlay of \code{y} on \code{x}.}
  \item{x = "afni", y = "afni"}{Produce overlay of \code{y} on \code{x}.}
 }
}
\author{Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
\seealso{
  \code{\link{image-methods}}, \code{\link{overlay-methods}}
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
