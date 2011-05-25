\name{SerialisableObject-class}
\Rdversion{1.1}
\docType{class}
\alias{SerialisableObject-class}
\alias{SerialisableObject}
\alias{show,SerialisableObject-method}

\title{Class "SerialisableObject"}
\description{
This reference class extends the standard \code{"\linkS4class{envRefClass}"} class, adding a function for simple serialisation of the data fields of an object. A serialised object may be deserialised using the \code{\link{deserialiseReferenceObject}} function.
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{serialise(file)}:}{If the \code{file} argument is missing, serialises the object to a list containing the (named) fields of the object, with its \code{"originalClass"} attribute set to the class name of the original object. If the file argument is supplied, this object is saved to the corresponding file name using the standard \code{\link{save}} function.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SerialisableObject")}: Show method for this class.}
	 }
}
\author{Jon Clayden}
\seealso{\code{\link{deserialiseReferenceObject}}, \code{\link{save}}.}
\keyword{classes}
