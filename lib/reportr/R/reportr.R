setOutputLevel <- function (level, usePrefix = NA)
{
    if (level %in% OL$Debug:OL$Error)
        options(outputLevel=level)
    if (usePrefix %in% c(TRUE,FALSE))
        options(useOutputPrefix=usePrefix)
}

report <- function (level, ..., default = NULL, showDepth = TRUE, outputErrors = FALSE)
{
    if (is.null(getOption("outputLevel")))
    {
        cat("INFO: Output level is not set; defaulting to \"Info\"\n")
        options(outputLevel=OL$Info)
    }
    
    outputLevel <- getOption("outputLevel")
    if ((level < OL$Question) && (outputLevel > level))
        return (invisible(NULL))
    
    reportFlags()
    
    prefixStrings <- c("DEBUG: ", "VERBOSE: ", "INFO: ", "WARNING: ")
    usePrefix <- getOption("useOutputPrefix")
    if (is.null(usePrefix))
        usePrefix <- TRUE
    
    callStrings <- as.character(sys.calls())
    runExperimentCallLoc <- which(callStrings %~% "^runExperiment")
    if (length(runExperimentCallLoc) == 1)
        callStrings <- callStrings[-seq_len(runExperimentCallLoc-1)]
    callStrings <- callStrings[setdiff(seq_along(callStrings), which(callStrings %~% "([tT]ryCatch|Error|cores|FUN)"))]
    stackDepth <- length(callStrings)

    nStars <- ifelse(showDepth, stackDepth, 0)
    leadingSpace <- ifelse(usePrefix && (nStars > 0), paste(rep("* ", nStars),collapse=""), "")
    leadingSpace <- paste(ifelse(isTRUE(getOption("outputPid")), paste("[",Sys.getpid(),"] ",sep=""), ""), leadingSpace, sep="")
    if ((level < OL$Question) && (outputLevel <= level))
        cat(paste(leadingSpace, ifelse(usePrefix,prefixStrings[level],""), ..., "\n", sep=""))
    else if (level == OL$Question)
    {
        if (outputLevel == OL$Error)
            return (default)
        else
        {
            ans <- readline(paste(leadingSpace, ifelse(usePrefix,"QUESTION: ",""), ..., " ", sep=""))
            return (ans)
        }
    }
    else if (level == OL$Error)
    {
        if (outputErrors && isTRUE(getOption("outputErrors")))
            cat(paste(leadingSpace, "ERROR: ", ..., "\n", sep=""))
        
        if (outputLevel == OL$Debug && !(stackDepth > 1 && callStrings[stackDepth-1] %~% "^(\\* )*output\\("))
        {
            cat("--- Begin stack trace ---\n")
            for (i in 1:length(callStrings))
                cat(rep("* ", i), callStrings[i], "\n", sep="")
            cat("---  End stack trace  ---\n")
        }
        stop(..., call.=FALSE)
    }
}

flag <- function (level, ...)
{
    currentFlag <- list(list(level=level, message=paste(...,sep="")))
    
    if (!exists(".ReportrFlags") || is.null(.ReportrFlags))
        .ReportrFlags <<- currentFlag
    else
        .ReportrFlags <<- c(.ReportrFlags, currentFlag)
}

reportFlags <- function ()
{
    if (exists(".ReportrFlags") && !is.null(.ReportrFlags))
    {
        levels <- unlist(lapply(.ReportrFlags, "[[", "level"))
        messages <- unlist(lapply(.ReportrFlags, "[[", "message"))
        
        # This is before the call to report() to avoid infinite recursion
        .ReportrFlags <<- NULL
        
        for (message in unique(messages))
        {
            locs <- which(messages == message)
            level <- max(levels[locs])
            if (length(locs) == 1)
                report(level, message, showDepth=FALSE)
            else
                report(level, paste("[x",length(locs),"] ",message,sep=""), showDepth=FALSE)
        }
    }
}
