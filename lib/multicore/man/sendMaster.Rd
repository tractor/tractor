\name{sendMaster}
\alias{sendMaster}
\title{
  Sends data from the child to to the master process
}
\description{
  \code{sendMaster} Sends data from the child to to the master process
}
\usage{
sendMaster(what)
}
\arguments{
\item{what}{data to send to the master process. If \code{what} is not
a raw vetor, \code{what} will be serialized into a raw vector. Do NOT
send an empty raw vector - it is reserved for internal use.}
}
\value{
  returns \code{TRUE}
}
\details{
  Any child process (created by \code{\link{fork}} directly or by
  \code{\link{parallel}} indirectly) can send data to the parent
  (master) process. Usually this is used to deliver results from the
  parallel child processes to the master process.
}
\seealso{
  \code{\link{parallel}}, \code{\link{fork}}
}
\author{Simon Urbanek}
\keyword{interface}
