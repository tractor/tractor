\name{output}
\alias{OL}
\alias{output}
\alias{flag}
\alias{reportFlags}
\alias{setOutputLevel}
\title{The TractoR message reporting system}
\description{
  Functions for working with TractoR's own system for reporting informative messages, warnings and errors. TractoR functions use this system in preference to the \code{\link{message}}, \code{\link{warning}} and \code{\link{stop}} functions.
}
\usage{
output(level, ..., default = NULL, showDepth = TRUE, toReport = FALSE)
flag(level, ...)
reportFlags()
setOutputLevel(level, usePrefix = NA)
}
\arguments{
  \item{level}{The level of output message to produce, or for \code{setOutputLevel}, the minimum level to display. See Details.}
  \item{\dots}{Objects which can be coerced to mode \code{character}. These will be printed with no space between them.}
  \item{default}{For questions, a default return value, to be used when the output level is above \code{OL$Question}.}
  \item{showDepth}{If \code{TRUE}, add a series of stars at the beginning of the output string to show the depth of the caller in the execution stack. If the \code{tractorUseOutputPrefix} option is \code{FALSE}, this parameter will have no effect.}
  \item{toReport}{If \code{TRUE}, report errors like other messages. By default this is \code{FALSE} because \code{stop} reports errors instead.}
  \item{usePrefix}{If \code{TRUE} or \code{FALSE}, set the \code{tractorUseOutputPrefix} option appropriately. \code{NA} means leave this option at its current value.}
}
\details{
  The \code{tractor.base} package uses its own system for reporting messages, which provides certain useful features over the standard R system, such as the incorporation of output consolidation, automatic generation of stack traces for debugging, and conditional reporting based on the current "output level".
  
  The output level is set by the \code{setOutputLevel} function, and governs whether a particular call to \code{output} will actually report anything. Output levels are described by the \code{OL} object, a list with components \code{Debug}, \code{Verbose}, \code{Info}, \code{Warning}, \code{Question} and \code{Error}; and any call to \code{output} using a level lower than the current output level will produce no output. If \code{output} is called before \code{setOutputLevel}, the output level will default to \code{OL$Info}.
  
  The \code{flag} function is called like output, but it stores messages for later reporting, like \code{\link{warning}}, rather than reporting them immediately. Stored messages are reported when \code{reportFlags} is called, at which point multiple instances of the same message are consolidated where appropriate.
  
  The \code{Question} output level is special in that it requests input from the user, using \code{\link{readline}}. The text argument then forms the text of the question, and \code{output} returns the text entered by the user.
  
  The call \code{output(OL$Error,\dots)} is largely similar to \code{stop(\dots)} in most cases, except that a stack trace will be printed first if the current output level is \code{Debug}. The R error condition is signalled in any case. No other conditions are signalled by \code{output}.
}
\value{
  These functions are mainly called for their side effects, but \code{output} returns a character vector of length one giving the user's response for message of level \code{Question}.
}
\author{Jon Clayden}
\seealso{\code{\link{condition}} for the normal R condition signalling framework.}
\examples{
setOutputLevel(OL$Warning)
output(OL$Info, "Test message")  # no output
setOutputLevel(OL$Info)
output(OL$Info, "Test message")  # prints the message

flag(OL$Warning, "Test warning")  # no output
reportFlags()  # prints the message

x <- output(OL$Question, "What is 2+2?")
output(OL$Info, ifelse(as.numeric(x)==4,"Correct!","Wrong"))
}
