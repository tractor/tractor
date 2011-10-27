\name{execute}
\alias{execute}
\alias{locateExecutable}
\title{Find or run an external executable file}
\description{
The \code{execute} function is a wrapper around the \code{\link{system}} function in base \R, which echoes the command being run (including the full path to the executable) if the reportr output level is \code{Debug}. \code{locateExecutable} simply returns the path to an executable file on the system \code{PATH}.
}
\usage{
execute(executable, paramString = NULL, errorOnFail = TRUE, silent = FALSE, ...)
locateExecutable(fileName, errorIfMissing = TRUE)
}
\arguments{
  \item{executable, fileName}{Name of the executable to run.}
  \item{paramString}{A character string giving the parameters to pass to the executable, if any.}
  \item{errorOnFail, errorIfMissing}{Logical value: should an error be produced if the executable can't be found?}
  \item{silent}{Logical value: should the executable be run without any output?}
  \item{\dots}{Additional arguments to \code{\link{system}}.
}
}
\value{
For \code{execute}, the return value of the underlying call to \code{\link{system}}. For \code{locateExecutable}, the location of the requested executable, or \code{NULL} if it could not be found.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. URL http://www.jstatsoft.org/v44/i08/.
}
\note{
These functions are designed for Unix systems and may not work on Windows.
}
\seealso{
\code{\link{system}}
}
