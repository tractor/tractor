\name{children}
\alias{children}
\alias{readChild}
\alias{readChildren}
\alias{selectChildren}
\alias{sendChildStdin}
\alias{kill}
\title{
  Functions for management of parallel children processes
}
\description{
  \code{children} returns all currently active children
 
  \code{readChild} reads data from a given child process

  \code{selectChildren} checks children for available data

  \code{readChildren} checks children for available data and reads
  from the first child that has available data

  \code{sendChildStdin} sends string (or data) to child's standard input

  \code{kill} sends a signal to a child process
}
\section{Warning}{
  This is a very low-level API for expert use only. If you are
  interested in user-level parallel execution use
  \code{\link{mclapply}}, \code{\link{parallel}} and friends instead.
}
\usage{
children()
readChild(child)
readChildren(timeout = 0)
selectChildren(children = NULL, timeout = 0)
sendChildStdin(child, what)
kill(process, signal = SIGINT)
}
\arguments{
\item{child}{child process (object of the class \code{childProcess}) or a
process ID (pid)}
\item{timeout}{timeout (in seconds, fractions supported) to wait
before giving up. Negative numbers mean wait indefinitely (strongly
discouraged as it blocks R and may be removed in the future).}
\item{children}{list of child processes or a single child process
object or a vector of process IDs or \code{NULL}. If \code{NULL}
behaves as if all currently known children were supplied.}
\item{what}{character or raw vector. In the former case elements are
collapsed using the newline chracter. (But no trailing newline is
added at the end!)}
\item{process}{process (object of the class \code{process}) or a
process ID (pid)}
\item{signal}{signal to send (one of \code{SIG...} constants -- see
\code{\link{signals}} -- or a valid integer signal number)}
}
\value{
  \code{children} returns a list of child processes (or an empty list)

  \code{readChild} and \code{readChildren} return a raw vector with a
  \code{"pid"} attribute if data were available, integer vector of
  length one with the process ID if a child terminated or \code{NULL}
  if the child no longer exists (no children at all for
  \code{readChildren}).

  \code{selectChildren} returns \code{TRUE} is the timeout was reached,
  \code{FALSE} if an error occurred (e.g. if the master process was
  interrupted) or an integer vector of process IDs with children that
  have data available.

  \code{sendChildStdin} sends given content to the standard input
  (stdin) of the child process. Note that if the master session was
  interactive, it will also be echoed on the standard output of the
  master process (unless disabled). The function is vector-compatible,
  so you can specify more than one child as a list or a vector of
  process IDs.

  \code{kill} returns \code{TRUE}.
}
\seealso{
  \code{\link{fork}}, \code{\link{sendMaster}},
  \code{\link{parallel}}, \code{\link{mclapply}}
}
\author{Simon Urbanek}
\keyword{interface}
