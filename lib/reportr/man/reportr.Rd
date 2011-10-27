\name{reportr}
\alias{reportr}
\alias{OL}
\alias{getOutputLevel}
\alias{setOutputLevel}
\alias{report}
\alias{flag}
\alias{ask}
\alias{reportFlags}
\alias{clearFlags}
\alias{withReportrHandlers}
\title{The reportr message reporting system}
\description{
  Functions for reporting informative messages, warnings and errors. These are provided as alternatives to the \code{\link{message}}, \code{\link{warning}} and \code{\link{stop}} functions in base R.
}
\usage{
getOutputLevel()
setOutputLevel(level)

report(level, ..., prefixFormat = NULL)
flag(level, ...)
ask(..., default = NULL, prefixFormat = NULL)

reportFlags()
clearFlags()

withReportrHandlers(expr)
}
\arguments{
  \item{level}{The level of output message to produce, or for \code{setOutputLevel}, the minimum level to display. See Details.}
  \item{\dots}{Objects which can be coerced to mode \code{character}. These will be printed with no space between them.}
  \item{prefixFormat}{The format of the string prepended to the message. See Details.}
  \item{default}{A default return value, to be used when the message is filtered out or the output level is above \code{OL$Question}.}
  \item{expr}{An expression to be evaluated.}
}
\details{
  The \code{reportr} system for reporting messages provides certain useful features over the standard R system, such as the incorporation of output consolidation, message filtering, automatic generation of stack traces for debugging, and conditional reporting based on the current ``output level''. All output is written to standard error (see \code{\link{stderr}}).
  
  The output level is set by the \code{setOutputLevel} function, and governs whether a particular call to \code{report} will actually report anything. Output levels are described by the \code{OL} object, a list with components \code{Debug}, \code{Verbose}, \code{Info}, \code{Warning}, \code{Question} and \code{Error}; and any call to \code{report} using a level lower than the current output level will produce no output. If \code{report} is called before \code{setOutputLevel}, the output level will default to \code{OL$Info} (with a message).
  
  The \code{flag} function is called like \code{report}, but it stores messages for later reporting, like \code{\link{warning}}, rather than reporting them immediately. Stored messages are reported when \code{report} is next called, at which point multiple instances of the same message are consolidated where possible. The user may also manually force stored messages to be reported by calling \code{reportFlags}, or remove them with \code{clearFlags}. Note that the output level at the time that \code{reportFlags} is called (implicitly or explicitly) will determine whether the flags are printed.
  
  The \code{ask} function requests input from the user using \code{\link{readline}}, at output level \code{Question}. The text argument then forms the text of the question, and \code{ask} returns the text entered by the user.
  
  The call \code{report(OL$Error,\dots)} is largely similar to \code{stop(\dots)} in most cases, except that a stack trace will be printed if the current output level is \code{Debug}. The "abort" restart is invoked in any case. No other standard conditions are signalled by \code{report}. Stack traces can be generated at lower output levels, if desired, by setting the \code{reportrStackTraceLevel} option.
  
  The \code{withReportrHandlers} function evaluates \code{expr} in a context in which R errors, warnings and messages will be handled by reportr, rather than by the standard R functions.
  
  The \code{prefixFormat} argument to \code{report} and \code{ask} controls how the output message is formatted. It takes the form of a \code{\link{sprintf}}-style format string, but with different expansions for percent-escapes. Specifically, \code{"\%d"} expands to a series of stars indicating the current stack depth; \code{"\%f"} gives the name of the function calling \code{report} or \code{ask}; \code{"\%l"} and \code{"\%L"} give lower and upper case versions of the level of the message, respectively; and \code{"\%p"} expands to the ID of the current R process (see \code{\link{Sys.getpid}}). The default is \code{"\%d\%L: "}, giving a prefix such as \code{"* * INFO: "}, but this default can be overridden by setting the \code{reportrPrefixFormat} option.
  
  A number of other options influence the output produced by reportr. \code{getOutputLevel} and \code{setOutputLevel} get and set the \code{reportrOutputLevel} option, which can be set directly if preferred. The options \code{reportrMessageFilterIn} and \code{reportrMessageFilterOut} can contain a single character string representing a Perl regular expression, in which case only messages which match (\code{reportrMessageFilterIn}) or do not match (\code{reportrMessageFilterOut}) the regular expression will be retained. Likewise, the \code{reportrStackFilterIn} and \code{reportrStackFilterOut} options filter the call stack.
}
\value{
  These functions are mainly called for their side effects, but \code{getOutputLevel} returns the current output level, \code{withReportrHandlers} returns the value of the evaluated expression, and \code{ask} returns a character vector of length one giving the user's response.
}
\author{Jon Clayden}
\seealso{\code{\link{message}}, \code{\link{warning}}, \code{\link{stop}} and \code{\link{condition}} for the normal R message and condition signalling framework.}
\examples{
setOutputLevel(OL$Warning)
report(OL$Info, "Test message")  # no output
setOutputLevel(OL$Info)
report(OL$Info, "Test message")  # prints the message

flag(OL$Warning, "Test warning")  # no output
flag(OL$Warning, "Test warning")  # repeated warning
reportFlags()  # consolidates the warnings and prints the message

name <- ask("What is your name?")
report(OL$Info, "Hello, ", name)
}
