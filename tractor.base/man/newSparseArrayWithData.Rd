\name{newSparseArrayWithData}
\alias{newSparseArrayWithData}
\title{Create a SparseArray object}
\description{
This function creates a \code{\linkS4class{SparseArray}} object from its constituent parts.
}
\usage{
newSparseArrayWithData(data, coordinates, dims)
}
\arguments{
  \item{data}{A vector of (nonzero) array elements.}
  \item{coordinates}{A matrix with as many rows as \code{data} has elements, containing the coordinates of each nonzero element in the array.}
  \item{dims}{The dimensions of the array.}
}
\value{
A \code{\linkS4class{SparseArray}} object.
}
\author{Jon Clayden}
