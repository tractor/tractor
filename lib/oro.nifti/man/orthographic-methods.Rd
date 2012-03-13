% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{orthographic-methods}
\docType{methods}
\alias{orthographic}
\alias{orthographic-methods}
\alias{orthographic,nifti-method}
\alias{orthographic,anlz-method}
\alias{orthographic,array-method}
\alias{orthographic,afni-method}
\alias{orthographic.nifti}
\title{Methods for Function `orthographic' in Package `dcemriS4'}
\description{
  Produce orthographic display for \code{nifti}, \code{anlz} and
  \code{afni} objects.
}
\usage{
\S4method{orthographic}{nifti}(x, y = NULL, xyz = NULL, w = 1, 
             col = gray(0:64/64), col.y = hotmetal(), zlim = NULL,
             zlim.y = NULL, crosshairs = TRUE, col.crosshairs = "red",
             xlab = "", ylab = "", axes = FALSE,
             oma = rep(0,4), mar = rep(0,4), bg = "black",
             text = NULL, text.color="white", text.cex = 2, ...)
}
\arguments{
  \item{x}{is an object of class \code{nifti} or similar.}
  \item{y}{is an object of class \code{nifti} or similar for the overlay.}
  \item{xyz}{is the coordinate for the center of the crosshairs.}
  \item{w}{is the time point to be displayed (4D arrays only).}
  \item{col}{is grayscale (by default).}
  \item{col.y}{is hotmetal (by default).}
  \item{zlim}{is the minimum and maximum \sQuote{z} values passed into
    \code{image}.}
  \item{zlim.y}{is the minimum and maximum \sQuote{z} values passed into
    \code{image} for the overlay.}
  \item{crosshairs}{is a logical value for the presence of crosshairs in
    all three orthogonal planes (default = TRUE).}
  \item{col.crosshairs}{is the color of the crosshairs (default = red).}
  \item{xlab}{is set to "" since all margins are set to zero.}
  \item{ylab}{is set to "" since all margins are set to zero.}
  \item{axes}{is set to \code{FALSE} since all margins are set to zero.}
  \item{oma}{is the size of the outer margins in the \code{par} function.}
  \item{mar}{is the number of lines of margin in the \code{par} function.}
  \item{bg}{is the background color in the \code{par} function.}
  \item{text}{allows the user to specify text to appear in the fourth
    (unused) pane.}
  \item{text.color}{is the color of the user-specified text (default =
    \dQuote{white}.}
  \item{text.cex}{is the size of the user-specified text (default = 2).}
  \item{\dots}{other arguments to the \code{image} function may be
    provided here.}
}
\section{Methods}{
  \describe{
    \item{x = "afni"}{Produce orthographic display for \code{x}.}
    \item{x = "anlz"}{Produce orthographic display for \code{x}.}
    \item{x = "array"}{Produce orthographic display for \code{x}.}
    \item{x = "nifti"}{Produce orthographic display for \code{x}.}
  }
}
\author{Brandon Whitcher \email{bjw34032@users.sourceforge.net}}
\seealso{
  \code{\link{image-methods}}, \code{\link{overlay-methods}}
}
%\examples{}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
