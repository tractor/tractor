\name{\%~\%}
\alias{\%~\%}
\title{Test a character vector for a match against a regular expression}
\description{
  Test if a character
}
\usage{
X \%~\% Y
}
\arguments{
  \item{X}{A character vector to test for a match.}
  \item{Y}{A character vector of length one representing a Perl regular expression to match against.}
}
\details{
  This function is infix shorthand for \code{regexpr(Y,X,perl=TRUE) != -1}.
}
\value{
  TRUE if \code{X} matches the regular expression described by \code{Y}.
}
\author{Jon Clayden}
\seealso{\code{\link{regexpr}}}
\examples{
c("foo", "foo bar") \%~\% "\\\\w+\\\\s+(\\\\w+)?"  # c(FALSE, TRUE)
}