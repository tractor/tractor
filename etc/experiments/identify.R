#@args session number
#@desc Identify the session directory corresponding to the specified number. This
#@desc correspondence depends on the session list for the experiment, which is usually
#@desc stored in a configuration file.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() < 1 || !isValidAs(Arguments[1], "integer"))
        output(OL$Error, "An integer argument must be given, specifying the session number required")
    
    sessionList <- getWithDefault("SessionList", NULL, "character", errorIfMissing=TRUE)

    loc <- as.integer(Arguments[1])
    nSessions <- length(sessionList)
    if (loc < 1 || loc > nSessions)
        output(OL$Error, "The specified session number is out of bounds: there are ", nSessions, " sessions in total")
    
    cat(paste(sessionList[loc], "\n", sep=""))
}
