% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_files.R
\docType{class}
\name{FileSet-class}
\alias{FileSet-class}
\alias{FileSet}
\title{The FileSet class}
\description{
This reference class manages a set of related files based on a common file
stem and required or suffixes. It supports operations such as copying,
moving, symlinking, validating and deleting files, ensuring that all
constituent files are handled consistently. It is designed for handling
alternative file formats or composite file types.
}
\section{Fields}{

\describe{
\item{\code{formats}}{A named list mapping format names to required file suffixes.}

\item{\code{validators}}{An optional named list of validation functions for some
or all of the supported formats.}

\item{\code{auxiliaries}}{A character vector of optional auxiliary file suffixes.}
}}


