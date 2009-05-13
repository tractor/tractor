\name{isValidAs}
\alias{isValidAs}
\title{Check whether an object can be coerced to a specified mode}
\description{
  Check whether an object can be coerced to a specified mode.
}
\usage{
isValidAs(value, mode)
}
\arguments{
  \item{value}{Any object.}
  \item{mode}{A mode name, given as a character vector of length one.}
}
\value{
  TRUE if all elements of the specified \code{value} can be successfully coerced to the given \code{mode}. FALSE otherwise.
}
\author{Jon Clayden}
\seealso{\code{\link{mode}}}
\examples{
isValidAs("1.2", "double")  # TRUE
isValidAs("1.2", "integer")  # TRUE, although the value will be truncated
isValidAs(c(7,9,11), "double")  # TRUE
isValidAs("text", "double")  # FALSE
}