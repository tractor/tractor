\name{multicore}
\alias{multicore}
\alias{process}
\alias{childProcess}
\alias{masterProcess}
\alias{parallelJob}
\title{
  multicore R package for parallel processing of R code
}
\description{
\emph{multicore} is an R package that provides functions for parallel
execution of R code on machines with multiple cores or CPUs. Unlike
other parallel processing methods all jobs share the full state of
R when spawned, so no data or code needs to be initialized. The
actual spawning is very fast as well since no new R instance needs to
be started.
}
\section{Pivotal functions}{
\code{\link{mclapply}} - parallelized verson of \code{\link{lapply}}

\code{\link{parallel}} and \code{\link{collect}} - functions to
evaluate R expressions in parallel and collect the results.
}
\section{Low-level functions}{
Those function should be used only by experienced users understanding
the interaction of the master (parent) process and the child processes
(jobs) as well as the system-level mechanics involved.

See \code{\link{fork}} help page for the principles of forking
parallel processes and system-level functions, \code{\link{children}}
and \code{\link{sendMaster}} help pages for management and
communication between the parent and child processes.
}
\section{Classes}{
\emph{multicore} defines a few informal (S3) classes:

\code{process} is a list with a named entry \code{pid} containing the
process ID.

\code{childProcess} is a subclass of \code{process} representing a
child process of the current R process. A child process is a special
process that can send messages to the parent process. The list may
contain additional entries for IPC (more precisely file descriptors),
however those are considered internal.

\code{masterProcess} is a subclass of \code{process} representing a
handle that is passed to a child process by \code{\link{fork}}.

\code{parallelJob} is a subclass of \code{childProcess} representing a
child process created using the \code{\link{parallel}} function. It
may (optionally) contain a \code{name} entry -- a character vector
of the length one as the name of the job.
}
\section{Options}{
By default functions that spawn jobs across cores use the
\code{"cores"} option (see \code{\link{options}}) to determine how
many cores (or CPUs) will be used (unless specified directly). If this
option is not set, \emph{multicore} uses by default as many cores as
there are available.

The number of available cores is determined on startup using the
(non-exported) \code{detectCores()} function. It should work on most
commonly used unix systems (Mac OS X, Linux, Solaris and IRIX), but
there is no standard way of determining the number of cores, so
please contact me (with \code{sessionInfo()} output and the test) if
you have tests for other platforms. If in doubt, use
\code{multicore:::detectCores(all.tests=TRUE)} to see whether your
platform is covered by one of the already existing tests. If multicore
cannot determine the number of cores (the above returns \code{NA}), it
will default to 8 (which should be fine for most modern desktop
systems).
}
\section{Warning}{
\emph{multicore} uses the \code{fork} system call to spawn a copy of
the current process which performs the compultations in
parallel. Modern operating systems use copy-on-write approach which
makes this so appealing for parallel computation since only objects
modified during the computation will be actually copied and all other
memory is directly shared.

However, the copy shares everything including any user interface
elements. This can cause havoc since let's say one window now suddenly
belongs to two processes. Therefore \emph{multicomp} should be
preferrably used in console R and code executed in parallel may
never use GUIs or on-screen devices.

An (experimental) way to avoid some such problems in some GUI
environments (those using pipes or sockets) is to use
\code{multicore:::closeAll()} in each child process immediately after
it is spawned.
}
\seealso{
  \code{\link{parallel}}, \code{\link{mclapply}},
  \code{\link{fork}}, \code{\link{sendMaster}}, \code{\link{children}}
  and \code{\link{signals}}
}
\author{Simon Urbanek}
\keyword{interface}
