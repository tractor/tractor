\name{printLabelledValues}
\alias{printLabelledValues}
\title{Pretty print labelled information}
\description{
This is a simple function to print a series of labels and associated data values, or key-value pairs.
}
\usage{
printLabelledValues(labels, values, outputLevel = OL$Info, leftJustify = FALSE)
}
\arguments{
  \item{labels}{A character vector of labels.}
  \item{values}{A character vector of values. Must have the same length as \code{labels}.}
  \item{outputLevel}{The output level to print the output to. See \code{\link{setOutputLevel}}, in the reportr package.}
  \item{leftJustify}{Logical value: if \code{TRUE} the labels will be left justified; otherwise they will be right justified.}
}
\value{
This function is called for its side effect.
}
\author{Jon Clayden}
\seealso{
\code{\link{setOutputLevel}} for the reportr output level system.
}
