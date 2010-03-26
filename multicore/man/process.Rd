\name{process}
\alias{processID}
\alias{print.process}
\title{
  Function to query objects of the class process
}
\description{
  \code{processID} returns the process IDs for the given processes. It
  raises an error if \code{process} is not an object of the class
  \code{\link{process}} or a list of such objects.

  \code{print} methods shows the process ID and its class name.
}
\usage{
processID(process)
\method{print}{process}(x, \dots)
}
\arguments{
\item{process}{process (object of the class \code{process}) or a list
of such objects.}
\item{x}{process to print}
\item{...}{ignored}
}
\value{
 \code{processID} returns an integer vector contatining the process IDs.

 \code{print} returns \code{NULL} invisibly
}
\seealso{
  \code{\link{fork}}
}
\author{Simon Urbanek}
\keyword{interface}
