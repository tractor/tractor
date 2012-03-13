% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{[<--methods}
\docType{methods}
\alias{[<--methods}
\alias{[<-,ANY,ANY,ANY,ANY-method}
\alias{[<-,nifti,ANY,ANY,ANY-method}
\alias{[<-,nifti,numeric,numeric,ANY-method}
\alias{[<-,nifti,ANY,missing,ANY-method}
\alias{[<-,nifti,numeric,missing,ANY-method}
\alias{[<-,nifti,missing,missing,array-method}
\title{ ~~ Methods for Function [<- in Package 'base' ~~}
\description{
 ~~ Methods for function \code{[<-} in Package 'base' ~~ }%'`]}
\section{Methods}{
  \describe{
    \item{x = "nifti", i = "ANY", j = "ANY", value = "ANY"}{Replaces the data at the provided co-ordinates with the value provided and updates the header}
    \item{x = "nifti", i = "numeric", j = "numeric", value = "ANY"}{Replaces the data at the provided co-ordinates with the value provided and updates the header}
\item{x = "nifti", i = "ANY", j = "missing", value = "ANY"}{Replaces the data row i of the provided nifti object with the value provided and updates the header}
\item{x = "nifti", i = "numeric", j = "missing", value = "ANY"}{Replaces the data row i of the provided nifti object with the value provided and updates the header}
\item{x = "nifti", i = "missing", j = "missing", value = "array"}{Replaces the data of the provided nifti object with the array provided and updates the header}
}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
