\name{getColourScale}
\alias{getColourScale}
\alias{interpolatePalette}
\title{Functions for working with colour scales or palettes}
\description{
The \code{getColourScale} function can be used to obtain a standard or customised colour scale for use in the package's image visualisation functions. A graded palette of colours between two or more key colours can be obtained using \code{interpolatePalette}.
}
\usage{
getColourScale(n)

interpolatePalette(colours, n, ...)
}
\arguments{
  \item{n}{For \code{getColourScale}, a number, colour name or list (see Details). For \code{interpolatePalette}, a single integer specifying the length of the interpolated palette.}
  \item{colours}{A vector of colours to interpolate between, using any format recognised by \code{\link{colours}}.}
  \item{\dots}{Additional arguments to \code{\link{colorRamp}}.}
}
\details{
Colour scales can be specified in any of three ways. Firstly, by a single number, representing a predefined colour scale. Currently valid values are 1 (greyscale, black background), 2 (red to yellow heat scale, red background), 3 (blue to red rainbow scale, blue background), 4 (blue to white to red diverging scale, white background), 5 (white to red, white background) and 6 (white to blue, white background). Secondly, a single colour name can be given (see \code{\link{colours}}); in this case the background will be black. This is useful for binary images. Thirdly and most flexibly, a list with two named elements can be given: \code{colours}, a vector of colours representing the colour scale, perhaps created using \code{\link{rgb}}; and \code{background}, a single colour representing the background.
}
\value{
For \code{getColourScale}, a list with elements
\item{colours}{A character-mode vector representing the colours in the scale, usually of length 100. This can be passed as a colour scale to R's plotting functions.}
\item{background}{A single character string representing the background colour.}
This can be passed as an argument to the package's image visualisation functions.

The \code{interpolatePalette} function returns a character-mode vector representing the colours in the interpolated scale.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{\code{\link{colours}}, \code{\link{rgb}}, \code{\link{colorRamp}}}
\examples{
getColourScale(1)

interpolatePalette(c("red","yellow"), 10)
}
