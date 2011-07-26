\name{\%~\%}
\alias{\%~\%}
\alias{\%!~\%}
\title{Test a character vector for a match against a regular expression}
\description{
  Test whether the elements of a character vector match, or don't match, a Perl regular expression.
}
\usage{
X \%~\% Y
X \%!~\% Y
}
\arguments{
  \item{X}{A character vector to test for matches.}
  \item{Y}{A character vector of length one representing a Perl regular expression to match against.}
}
\details{
  \code{\%~\%} is infix shorthand for \code{regexpr(Y,X,perl=TRUE) != -1}.
}
\value{
  For \code{\%~\%}, TRUE for each element of \code{X} which matches the regular expression described by \code{Y}, and FALSE for every other element. For \code{\%!~\%}, the opposite.
}
\author{Jon Clayden}
\seealso{\code{\link{regexpr}}}
\examples{
c("foo", "foo bar") \%~\% "\\\\w+\\\\s+(\\\\w+)?"  # c(FALSE, TRUE)
c("foo", "foo bar") \%!~\% "\\\\w+\\\\s+(\\\\w+)?"  # c(TRUE, FALSE)
}