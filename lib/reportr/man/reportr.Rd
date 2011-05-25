\name{reportr}
\alias{reportr}
\alias{OL}
\alias{report}
\alias{flag}
\alias{reportFlags}
\alias{setOutputLevel}
\title{The reportr message reporting system}
\description{
  Functions for reporting informative messages, warnings and errors. These are provided as alternatives to the \code{\link{message}}, \code{\link{warning}} and \code{\link{stop}} functions in base R.
}
\usage{
setOutputLevel(level, usePrefix = NA)
report(level, ..., default = NULL, showDepth = TRUE, outputErrors = FALSE)
flag(level, ...)
reportFlags()
}
\arguments{
  \item{level}{The level of output message to produce, or for \code{setOutputLevel}, the minimum level to display. See Details.}
  \item{usePrefix}{If \code{TRUE} or \code{FALSE}, set the \code{useOutputPrefix} option appropriately. \code{NA} means leave this option at its current value.}
  \item{\dots}{Objects which can be coerced to mode \code{character}. These will be printed with no space between them.}
  \item{default}{For questions, a default return value, to be used when the output level is above \code{OL$Question}.}
  \item{showDepth}{If \code{TRUE}, add a series of stars at the beginning of the output string to show the depth of the caller in the execution stack. If the \code{useOutputPrefix} option is \code{FALSE}, this parameter will have no effect.}
  \item{outputErrors}{If \code{TRUE}, output errors like other messages. By default this is \code{FALSE} because \code{stop} produces error messages instead.}
}
\details{
  The \code{reportr} system for reporting messages provides certain useful features over the standard R system, such as the incorporation of output consolidation, automatic generation of stack traces for debugging, and conditional reporting based on the current "output level".
  
  The output level is set by the \code{setOutputLevel} function, and governs whether a particular call to \code{report} will actually report anything. Output levels are described by the \code{OL} object, a list with components \code{Debug}, \code{Verbose}, \code{Info}, \code{Warning}, \code{Question} and \code{Error}; and any call to \code{report} using a level lower than the current output level will produce no output. If \code{report} is called before \code{setOutputLevel}, the output level will default to \code{OL$Info}.
  
  The \code{flag} function is called like \code{report}, but it stores messages for later reporting, like \code{\link{warning}}, rather than reporting them immediately. Stored messages are reported when \code{report} is next called, at which point multiple instances of the same message are consolidated where possible. The user may also manually force stored messages to be reported by calling \code{reportFlags}.
  
  The \code{Question} output level is special in that it requests input from the user, using \code{\link{readline}}. The text argument then forms the text of the question, and \code{report} returns the text entered by the user.
  
  The call \code{report(OL$Error,\dots)} is largely similar to \code{stop(\dots)} in most cases, except that a stack trace will be printed first if the current output level is \code{Debug}. The R error condition is signalled in any case. No other conditions are signalled by \code{report}.
}
\value{
  These functions are mainly called for their side effects, but \code{report} returns a character vector of length one giving the user's response for messages of level \code{Question}.
}
\author{Jon Clayden}
\seealso{\code{\link{condition}} for the normal R condition signalling framework.}
\examples{
setOutputLevel(OL$Warning)
report(OL$Info, "Test message")  # no output
setOutputLevel(OL$Info)
report(OL$Info, "Test message")  # prints the message

flag(OL$Warning, "Test warning")  # no output
flag(OL$Warning, "Test warning")  # repeated warning
reportFlags()  # consolidates the warnings and prints the message

x <- report(OL$Question, "What is 2+2?")
report(OL$Info, ifelse(as.numeric(x)==4,"Correct!","Wrong"))
}
