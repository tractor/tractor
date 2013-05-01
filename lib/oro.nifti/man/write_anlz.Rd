% Part of the oro.nifti package for R
% Distributed under the BSD license: see oro.nifti/COPYING
%
% $Id: $

\name{writeANALYZE-methods}
\docType{methods}
\alias{writeANALYZE-methods}
\alias{writeANALYZE,anlz-method}
\alias{writeANALYZE}
\title{writeANALYZE}
\description{
  This function saves an Analyze-class object to a single binary file in
  Analyze format.
}
\usage{
\S4method{writeANALYZE}{anlz}(aim, filename, gzipped = TRUE, verbose = FALSE,
            warn = -1)
}
\arguments{
  \item{aim}{is an object of class \code{anlz}.}
  \item{filename}{is the path and file name to save the Analyze file
    pair (.hdr,img) \bold{without} the suffixes.}
  \item{gzipped}{is a character string that enables exportation of
    compressed (.gz) files (default = \code{TRUE}).}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{\link{options}} for more details.}
}
\section{Methods}{
  \describe{
  \item{object = "anlz"}{Write ANALYZE volume to disk.}
  }
}
\details{
  The \code{writeANALYZE} function utilizes the internal \code{writeBin}
  and \code{writeChar} command to write information to a binary file.
}
\value{
  Nothing.
}
\references{
  Analyze 7.5\cr
  \url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}
}
\seealso{
  \code{\link{writeAFNI}}, \code{\link{writeNIfTI}}
}
\examples{
norm <- dnorm(seq(-5, 5, length=32), sd=2)
norm <- (norm-min(norm)) / max(norm-min(norm))
img <- outer(outer(norm, norm), norm)
img <- round(255*img)
img[17:32,,] <- 255 - img[17:32,,]
img.anlz <- anlz(img) # create Analyze object

writeANALYZE(img.anlz, "test-anlz-image-uint8", verbose=TRUE)
## These files should be viewable in, for example, FSLview
## Make sure you adjust the min/max values for proper visualization
data <- readANALYZE("test-anlz-image-uint8", verbose=TRUE)
image(img.anlz, oma=rep(2,4), bg="white")
image(data, oma=rep(2,4), bg="white")
abs.err <- abs(data - img.anlz)
image(as(abs.err, "anlz"), zlim=range(img.anlz), oma=rep(2,4), bg="white")

\dontrun{
## Loop through all possible data types
datatypes <- list(code=c(2, 4, 8, 16, 64),
                  name=c("uint8", "int16", "int32", "float", "double"))
equal <- vector("list")
for (i in 1:length(datatypes$code)) {
  fname <- paste("test-anlz-image-", datatypes$name[i], sep="")
  rm(img.anlz)
  img.anlz <- anlz(img, datatype=datatypes$code[i])
  writeANALYZE(img.anlz, fname)
  equal[[i]] <- all(readANALYZE(fname) == img)
}
names(equal) <- datatypes$name
unlist(equal)
}
}
\author{
  Brandon Whitcher \email{bwhitcher@gmail.com}
}
\keyword{file}
\keyword{methods}
