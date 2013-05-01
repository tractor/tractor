\name{bitFlip}
\alias{bitFlip}
\title{Binary Flip (Not) Operator}
\description{
  Binary flop (not) operator.
}
\usage{
bitFlip(a, bitWidth=32)
}
\arguments{
  \item{a}{numeric vector.}
  \item{bitWidth}{scalar integer between 0 and 32.}
}
\value{
  binary numeric vector of the same length as \code{a} masked with
  (2^\code{bitWidth})-1.  \code{\link{NA}} is returned for any value of
  \code{a} that is not finite or whose magnitude is greater or equal to
  \eqn{2^{32}}.
}
\author{Steve Dutky}
\seealso{\code{\link{bitShiftL}}, \code{\link{bitXor}}, etc.}
\examples{
 stopifnot(
  bitFlip(-1) == 0,
  bitFlip(0 ) == 4294967295,
  bitFlip(0, bitWidth=8) == 255
 )
}
\keyword{arith}
\keyword{utilities}
