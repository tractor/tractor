\name{implode}
\alias{implode}
\title{Create a character string by concatenating the elements of a vector}
\description{
  Create a character string by concatenating the elements of a vector, using a separator and optional final separator.
}
\usage{
implode(strings, sep = "", finalSep = NULL)
}
\arguments{
  \item{strings}{A vector, which will be coerced to mode \code{character}.}
  \item{sep}{A unit length character vector giving the separator to insert between elements.}
  \item{finalSep}{An optional unit length character vector giving the separator to insert between the final two elements.}
}
\value{
  A character vector of length one.
}
\author{Jon Clayden}
\seealso{\code{\link{paste}}}
\examples{
implode(1:3, ", ")  # "1, 2, 3"
implode(1:3, ", ", " and ")  # "1, 2 and 3"
implode(1:2, ", ", " and ")  # "1 and 2"
}