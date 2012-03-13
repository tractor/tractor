% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{image-methods}
\docType{methods}
\alias{image.nifti}
\alias{image-methods}
\alias{image,ANY-method}
\alias{image,afni-method}
\alias{image,anlz-method}
\alias{image,nifti-method}
\title{Methods for Function `image'}
\description{
  Produce \dQuote{lightbox} layout of images for \code{nifti},
  \code{anlz} and \code{afni} objects.
}
\usage{
\S4method{image}{nifti}(x, z=1, w=1, col=gray(0:64/64),
      plane=c("axial", "coronal", "sagittal"),
      plot.type=c("multiple","single"), zlim=NULL, xlab="",
      ylab="", axes=FALSE, oma=rep(0,4), mar=rep(0,4),
      bg="black", ...)
%\S4method{image}{anlz}(x, z=1, w=1, col=gray(0:64/64),
%      plot.type=c("multiple","single"), zlim=NULL, xlab="",
%      ylab="", axes=FALSE, oma=rep(0,4), mar=rep(0,4),
%      bg="black", ...)
}
\arguments{
  \item{x}{is an object of class \code{nifti} or similar.}
  \item{z}{is the slice to be displayed (ignored when \code{plot.type =
      "multiple"}).}
  \item{w}{is the time point to be displayed (4D arrays only).}
  \item{col}{is grayscale (by default).}
  \item{plane}{is the plane of acquisition to be displayed (choices are
    \sQuote{axial}, \sQuote{coronal}, \sQuote{sagittal}).}
  \item{plot.type}{allows the choice between all slices being displayed,
    in a matrix (left-to-right, top-to-bottom), or a single slice.}
  \item{zlim}{is set to \code{NULL} by default and utilizes the internal
    image range.}
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
  Uses the S3 generic function \code{image}, with medical-image friendly
  settings, to display \code{nifti}, \code{anlz} and \code{afni} class
  objects in a \dQuote{lightbox} layout.
}
%\value{}
\section{Methods}{
  \describe{
    \item{x = "ANY"}{Generic function: see \code{\link[graphics]{image}}.}
    \item{x = "nifti"}{Produce images for \code{x}.}
    \item{x = "anlz"}{Produce images for \code{x}.}
    \item{x = "afni"}{Produce images for \code{x}.}
  }
}
%\references{}
\author{Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
\seealso{
  \code{\link{orthographic-methods}}, \code{\link{overlay-methods}}
}
%\examples{}
\keyword{methods}
