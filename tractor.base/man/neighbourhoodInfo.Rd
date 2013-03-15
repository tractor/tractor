\name{neighbourhoodInfo}
\alias{neighbourhoodInfo}
\alias{createNeighbourhoodInfo}
\title{Class representing a cuboidal neighbourhood in an image}
\description{
  This class represents a cuboidal region of an image, with a centre and a fixed voxel width.
}
\usage{
createNeighbourhoodInfo(width, dim = 3, centre = rep(0, dim))
}
\arguments{
  \item{width}{An integer voxel width. Must be odd.}
  \item{dim}{An integer giving the dimensionality of the neighbourhood. Currently must be 3.}
  \item{centre}{A numeric vector giving the centre voxel of the neighbourhood. Must have exactly \code{dim} elements.}
}
\value{
  \code{createNeighbourhoodInfo} returns a list with class \code{"neighbourhoodInfo"} and elements
  \item{width}{Copied from the \code{width} argument.}
  \item{dim}{Copied from the \code{dim} argument.}
  \item{centre}{Copied from the \code{centre} argument.}
  \item{vectors}{\code{dim} x \code{width^dim} matrix whose columns give the locations of each point in the neighbourhood.}
  \item{innerProducts}{A square, symmetric matrix of inner products between every location in the neighbourhood and every other.}
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
