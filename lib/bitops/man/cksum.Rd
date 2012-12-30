\name{cksum}
\alias{cksum}
\title{Compute Check Sum}
\description{
  Return a cyclic redundancy checksum for each element in the argument.
}
\usage{
cksum(a)
}
\arguments{
  \item{a}{coerced to character vector}
}
\details{
  \code{\link{NA}}'s appearing in the argument are returned as \code{NA}'s.

  The default calculation is identical to that given in pseudo-code in the
  ACM article (in the References).
}
\value{
  numeric vector of the same length as \code{a}.
}
\references{
  Fashioned from \code{cksum(1)} UNIX command line utility, i.e.,
  \code{man cksum}.

  Dilip V. Sarwate (1988)
  Computation of Cyclic Redundancy Checks Via Table Lookup,
  \emph{Communications of the ACM} \bold{31}, 8, 1008--1013.
}
\author{Steve Dutky \email{sdutky@terpalum.umd.edu}
}
\seealso{\code{\link{bitShiftL}}, \code{\link{bitAnd}}, etc.
}
\examples{
   b <- "I would rather have a bottle in front of me than frontal lobotomy\n"
 stopifnot(cksum(b) == 1342168430)
 (bv <- strsplit(b, " ")[[1]])
 cksum(bv) # now a vector of length 13
}
\keyword{arith}
\keyword{utilities}
