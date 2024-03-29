% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{locateExecutable}
\alias{locateExecutable}
\alias{execute}
\title{Find or run an external executable file}
\usage{
locateExecutable(fileName, errorIfMissing = TRUE)

execute(executable, params = NULL, errorOnFail = TRUE, silent = FALSE,
  ...)
}
\arguments{
\item{executable, fileName}{Name of the executable to run.}

\item{params}{A character vector giving the parameters to pass to the
executable, if any. Elements will be separated by a space.}

\item{errorOnFail, errorIfMissing}{Logical value: should an error be produced
if the executable can't be found?}

\item{silent}{Logical value: should the executable be run without any
output?}

\item{\dots}{Additional arguments to \code{\link{system}}.}
}
\value{
For \code{execute}, the return value of the underlying call to
  \code{\link{system2}}. For \code{locateExecutable}, the location of the
  requested executable, or \code{NULL} if it could not be found.
}
\description{
The \code{execute} function is a wrapper around the \code{\link{system2}}
function in base, which additionally echoes the command being run (including
the full path to the executable) if the reportr output level is
\code{Debug}. \code{locateExecutable} simply returns the path to an
executable file on the system \code{PATH}.
}
\note{
These functions are designed for Unix systems and may not work on
  Windows.
}
\references{
Please cite the following reference when using TractoR in your
work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
}
\seealso{
\code{\link{system2}}
}
\author{
Jon Clayden
}
