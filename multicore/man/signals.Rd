\name{signals}
\alias{signals}
\alias{SIGALRM}
\alias{SIGCHLD}
\alias{SIGHUP}
\alias{SIGINFO}
\alias{SIGINT}
\alias{SIGKILL}
\alias{SIGQUIT}
\alias{SIGSTOP}
\alias{SIGTERM}
\alias{SIGUSR1}
\alias{SIGUSR2}
\title{
  Signal constants (subset)
}
\description{
\code{SIGALRM} alarm clock

\code{SIGCHLD} to parent on child stop or exit

\code{SIGHUP} hangup

\code{SIGINFO} information request

\code{SIGINT} interrupt

\code{SIGKILL} kill (cannot be caught or ignored)

\code{SIGQUIT} quit

\code{SIGSTOP} sendable stop signal not from tty

\code{SIGTERM} software termination signal from kill

\code{SIGUSR1} user defined signal 1

\code{SIGUSR2} user defined signal 2
}
\details{
  See \code{man signal} in your OS for details. The above codes can be
  used in conjunction with the \code{\link{kill}} function to send
  signals to processes.
}
\seealso{
  \code{\link{kill}}
}
\author{Simon Urbanek}
\keyword{interface}
