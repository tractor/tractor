\name{fork}
\alias{fork}
\alias{exit}
\title{
  Fork a copy of the current R process
}
\description{
  \code{fork} creates a new child process as a copy of the current R process
 
  \code{exit} closes the current child process, informing the master process as necessary
}
\section{Warning}{
  This is a very low-level API for expert use only. If you are
  interested in user-level parallel execution use
  \code{\link{mclapply}}, \code{\link{parallel}} and friends instead.
}
\usage{
fork()
exit(exit.code = 0L, send = NULL)
}
\arguments{
\item{exit.code}{process exit code. Currently it is not used by
multicore, but other applciations might. By convention 0 signifies
clean exit, 1 an error.}
\item{send}{if not \code{NULL} send this data before exiting
(equivalent to using \code{\link{sendMaster}})}
}
\value{
  \code{fork} returns an object of the class \code{childProcess} (to
  the master) and \code{masterProcess} (to the child).

  \code{exit} never returns
}
\details{
  The \code{fork} function provides an interface to the \code{fork}
  system call. In addition it sets up a pipe between the master and
  child process that can be used to send data from the child process
  to the master (see \code{\link{sendMaster}}) and child's stdin is
  re-mapped to another pipe held by the master process (see
  \code{link{sendChildStdin}}).

  If you are not familiar with the \code{fork} system call, do not
  use this function since it leads to very complex inter-process
  interactions among the R processes involved.

  In a nutshell \code{fork} spawns a copy (child) of the current
  process, that can work in parallel to the master (parent)
  process. At the point of forking both processes share exactly the
  same state including the workspace, global options, loaded packages
  etc. Forking is relatively cheap in modern operating systems and no
  real copy of the used memory is created, instead both processes
  share the same memory and only modified parts are copied. This makes
  \code{fork} an ideal tool for parallel processing since there is no
  need to setup the parallel working environment, data and code is
  shared automatically from the start.

  It is \emph{strongly discouraged} to use \code{fork} in GUI or
  embedded environments, because it leads to several processes sharing
  the same GUI which will likely cause chaos (and possibly
  crashes). Child processes should never use on-screen graphics
  devices.
}
\note{
  Windows opearting system lacks the \code{fork} system call so it
  cannot be used with multicore.
}
\seealso{
  \code{\link{parallel}}, \code{\link{sendMaster}}
}
\examples{
  p <- fork()
  if (inherits(p, "masterProcess")) {
    cat("I'm a child! ", Sys.getpid(), "\n")
    exit(,"I was a child")
  }
  cat("I'm the master\n")
  unserialize(readChildren(1.5))
}
\author{Simon Urbanek}
\keyword{interface}
