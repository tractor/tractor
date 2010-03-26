\name{parallel}
\alias{parallel}
\alias{collect}
\alias{mcparallel}
\title{
  Evaluate an expression asynchronously in a separate process
}
\description{
  \code{parallel} starts a parallel process which evaluates the given expression.

  \code{mcparallel} is a synonym for \code{parallel} that can be used
  at top level if \code{parallel} is masked by other packages. It
  should not be used in other packages since it's just a shortcut for
  importing \code{multicore::parallel}.

  \code{collect} collects results from parallel processes.
}
\usage{
parallel(expr, name, mc.set.seed = FALSE, silent = FALSE)
mcparallel(expr, name, mc.set.seed = FALSE, silent = FALSE)
collect(jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
}
\arguments{
\item{expr}{expression to evaluate (do \emph{not} use any on-screen
devices or GUI elements in this code)}
\item{name}{an optional name (character vector of length one) that can be associated with the job.}
\item{mc.set.seed}{if set to \code{TRUE} then the random number
generator is seeded such that it is different from any other
process. Otherwise it will be the same as in the current R session.}
\item{silent}{if set to \code{TRUE} then all output on stdout will be
suppressed (stderr is not affected).}
\item{jobs}{list of jobs (or a single job) to collect results
for. Alternatively \code{jobs} can also be an integer vector of
process IDs. If omitted \code{collect} will wait for all currently
existing children.}
\item{wait}{if set to \code{FALSE} it checks for any results that are
available within \code{timeout} seconds from now, otherwise it waits
for all specified jobs to finish.}
\item{timeout}{timeout (in seconds) to check for job results - applies
only if \code{wait} is \code{FALSE}.}
\item{intermediate}{\code{FALSE} or a function which will be called while
\code{collect} waits for results. The function will be called with one
parameter which is the list of results received so far.}
}
\value{
  \code{parallel} returns an object of the class \code{parallelJob}
  which is in turn a \code{childProcess}.

  \code{collect} returns any results that are available in a list. The
  results will have the same order as the specified jobs. If there are
  multiple jobs and a job has a name it will be used to name the
  result, otherwise its process ID will be used.
}
\details{
  \code{parallel} evaluates the \code{expr} expression in parallel to
  the current R process. Everything is shared read-only (or in fact
  copy-on-write) between the parallel process and the current process,
  i.e. no side-effects of the expression affect the main process. The
  result of the parallel execution can be collected using
  \code{collect} function.

  \code{collect} function collects any available results from parallel
  jobs (or in fact any child process). If \code{wait} is \code{TRUE}
  then \code{collect} waits for all specified jobs to finish before
  returning a list containing the last reported result for each
  job. If \code{wait} is \code{FALSE} then \code{collect} merely
  checks for any results available at the moment and will not wait for
  jobs to finish. If \code{jobs} is specified, jobs not listed there
  will not be affected or acted upon.

  Note: If \code{expr} uses low-level \code{multicore} functions such
  as \code{\link{sendMaster}} a single job can deliver results
  multiple times and it is the responsibility of the user to interpret
  them correctly. \code{collect} will return \code{NULL} for a
  terminating job that has sent its results already after which the
  job is no longer available.
}
\seealso{
  \code{\link{mclapply}}, \code{\link{sendMaster}}
}
\examples{
  p <- parallel(1:10)
  q <- parallel(1:20)
  collect(list(p, q)) # wait for jobs to finish and collect all results

  p <- parallel(1:10)
  collect(p, wait=FALSE, 10) # will retrieve the result (since it's fast)
  collect(p, wait=FALSE) # will signal the job as terminating
  collect(p, wait=FALSE) # there is no such job

  # a naive parallelized lapply can be created using parallel alone:
  jobs <- lapply(1:10, function(x) parallel(rnorm(x), name=x))
  collect(jobs)
}
\author{Simon Urbanek}
\keyword{interface}
