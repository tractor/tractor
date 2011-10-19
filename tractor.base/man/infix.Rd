\name{\%~\%}
\alias{\%~\%}
\title{Test a character vector for a match against a regular expression}
\description{
  Test if a the elements of a character vector match a Perl regular expression.
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
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic Resonance Imaging and Tractography with R. Journal of Statistical Software, 44(8), 1-18. URL http://www.jstatsoft.org/v44/i08/.}
\seealso{\code{\link{regexpr}}}
\examples{
c("foo", "foo bar") \%~\% "\\\\w+\\\\s+(\\\\w+)?"  # c(FALSE, TRUE)
}