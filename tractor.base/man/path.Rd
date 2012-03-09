\name{path manipulation}
\alias{ensureFileSuffix}
\alias{expandFileName}
\alias{relativePath}
\title{
Functions for file name and path manipulation
}
\description{
Functions for expanding file paths, finding relative paths and ensuring that a file name has the required suffix.
}
\usage{
ensureFileSuffix(fileName, suffix, strip = NULL)
expandFileName(fileName, base = getwd())
relativePath(path, referencePath)
}
\arguments{
  \item{fileName}{A character vector of file names.}
  \item{suffix}{A character vector of file suffixes, which will be recycled if shorter than \code{fileName}.}
  \item{strip}{A character vector of suffixes to remove before appending \code{suffix}. The intended suffix does not need to be given here, as the function will not append it if the specified file name already has the correct suffix.}
  \item{base}{If \code{fileName} is a relative path, this option gives the base directory which the path is relative to. If \code{fileName} is an absolute path, this argument is ignored.}
  \item{path, referencePath}{Character strings representing file paths.}
}
\value{
The \code{ensureFileSuffix} function returns the specified file names with the requested suffixes appended. \code{expandFileName} returns the full path to the specified file name, collapsing \code{".."} elements if appropriate. \code{relativePath} returns the specified \code{path}, expressed relative to \code{referencePath}.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{
\code{\link{path.expand}} performs some of what \code{expandFileName} does.
}
