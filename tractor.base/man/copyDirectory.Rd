\name{copyDirectory}
\alias{copyDirectory}
\title{
Copy a directory and its contents
}
\description{
This function copies a directory and its contents from one location to another. Unlike \code{\link{file.copy}}, the target directory doesn't have to exist, and the copied directory can have any name.
}
\usage{
copyDirectory(from, to, allFiles = TRUE, deleteOriginal = FALSE)
}
\arguments{
  \item{from}{A string giving the source directory.}
  \item{to}{A string giving the target directory. If it doesn't exist it will be created and filled with the contents of the source directory. If it does exist, a new subdirectory with the same \code{\link{basename}} as the source directory will be created, and the files put there (as with \code{\link{file.copy}}).}
  \item{allFiles}{Logical value: if \code{TRUE}, hidden files (with names beginning with a period) will be included in the copy.}
  \item{deleteOriginal}{Logical value: if \code{TRUE}, the source directory will be deleted if the copy is successful.}
}
\value{
\code{TRUE} if all files were copied successfully; \code{FALSE} otherwise.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{
\code{\link{file.copy}}
}
