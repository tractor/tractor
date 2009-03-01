\name{equivalent}
\alias{equivalent}
\title{Test two numeric vectors for equivalence}
\description{
  Test two numeric vectors for equivalence.
}
\usage{
equivalent(x, y, signMatters = TRUE, ...)
}
\arguments{
  \item{x}{The first numeric vector.}
  \item{y}{The second numeric vector.}
  \item{signMatters}{Logical value: if FALSE then equivalence in absolute value is sufficient.}
  \item{\dots}{Additional arguments to \code{\link{all.equal}}, notably \code{tolerance}.}
}
\details{
  This function is a wrapper for \code{isTRUE(all.equal(x,y,\dots))}, but with the additional capability of doing sign-insensitive comparison.
}
\value{
  \code{TRUE} if all elements of \code{x} match all elements of \code{y} to within tolerance, ignoring signs if required. \code{FALSE} otherwise.
}
\author{Jon Clayden}
\seealso{\code{\link{all.equal}}}
\examples{
equivalent(c(-1,1), c(1,1))  # FALSE
equivalent(c(-1,1), c(1,1), signMatters=FALSE)  # TRUE
equivalent(1:2, 2:3, tolerance=2)  # TRUE
}
