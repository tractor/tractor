setOutputLevel <- function (level, usePrefix = NA)
{
    if (level %in% OL$Debug:OL$Error)
        options(tractorOutputLevel=level)
    if (usePrefix %in% c(TRUE,FALSE))
        options(tractorUseOutputPrefix=usePrefix)
}

output <- function (level, ..., default = NULL, showDepth = TRUE, toReport = FALSE)
{
    if (is.null(getOption("tractorOutputLevel")))
    {
        cat("INFO: Output level is not set; defaulting to \"Info\"\n")
        options(tractorOutputLevel=OL$Info)
    }
    
    outputLevel <- getOption("tractorOutputLevel")
    if ((level < OL$Question) && (outputLevel > level))
        return (invisible(NULL))
    
    reportFlags()
    
    prefixStrings <- c("DEBUG: ", "VERBOSE: ", "INFO: ", "WARNING: ")
    usePrefix <- getOption("tractorUseOutputPrefix")
    if (is.null(usePrefix))
        usePrefix <- TRUE
    
    callStrings <- as.character(sys.calls())
    runExperimentCallLoc <- which(callStrings %~% "^runExperiment")
    if (length(runExperimentCallLoc) == 1)
        callStrings <- callStrings[-seq_len(runExperimentCallLoc-1)]
    callStrings <- callStrings[setdiff(seq_along(callStrings), which(callStrings %~% "([tT]ryCatch|Error)"))]
    stackDepth <- length(callStrings)

    nStars <- ifelse(showDepth, stackDepth, 0)
    leadingSpace <- ifelse(usePrefix && (nStars > 0), implode(rep("* ", nStars)), "")
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
        if (toReport && isTRUE(getOption("tractorOutputErrors")))
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
    
    if (is.null(.TractorFlags))
        .TractorFlags <<- currentFlag
    else
        .TractorFlags <<- c(.TractorFlags, currentFlag)
}

reportFlags <- function ()
{
    if (!is.null(.TractorFlags))
    {
        levels <- unlist(lapply(.TractorFlags, "[[", "level"))
        messages <- unlist(lapply(.TractorFlags, "[[", "message"))
        
        # This is before the call to output() to avoid infinite recursion
        .TractorFlags <<- NULL
        
        for (message in unique(messages))
        {
            locs <- which(messages == message)
            level <- max(levels[locs])
            if (length(locs) == 1)
                output(level, message, showDepth=FALSE)
            else
                output(level, paste("[x",length(locs),"] ",message,sep=""), showDepth=FALSE)
        }
    }
}
