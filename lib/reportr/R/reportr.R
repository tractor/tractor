setOutputLevel <- function (level)
{
    if (level %in% OL$Debug:OL$Error)
        options(reportrOutputLevel=level)
    invisible(NULL)
}

getOutputLevel <- function ()
{
    if (is.null(getOption("reportrOutputLevel")))
    {
        setOutputLevel(OL$Info)
        report(OL$Info, "Output level is not set; defaulting to \"Info\"", prefixFormat="")
        return (OL$Info)
    }
    else
        return (getOption("reportrOutputLevel"))
}

withReportrHandlers <- function (expr)
{
    withCallingHandlers(expr, message=function (m) {
        report(OL$Info, sub("\n$","",m$message,perl=TRUE))
        invokeRestart("muffleMessage")
    }, warning=function (w) {
        flag(OL$Warning, w$message)
        invokeRestart("muffleWarning")
    }, error=function (e) {
        if (is.null(e$call))
            report(OL$Error, e$message)
        else
            report(OL$Error, e$message, " (in \"", as.character(e$call)[1], "(", paste(as.character(e$call)[-1],collapse=", "), ")\")")
    })
}

.getCallStack <- function ()
{
    callStrings <- as.character(sys.calls())
    
    handlerFunLoc <- which(callStrings %~% "^withReportrHandlers\\(")
    if (length(handlerFunLoc) > 0)
        callStrings <- callStrings[-seq_len(handlerFunLoc[length(handlerFunLoc)]+1)]
    
    reportrFunLoc <- which(callStrings %~% "^(ask|flag|report|reportFlags)\\(")
    if (length(reportrFunLoc) > 0)
        callStrings <- callStrings[-(reportrFunLoc[length(reportrFunLoc)]:length(callStrings))]
    
    if (!is.null(getOption("reportrStackFilterIn")))
        callStrings <- callStrings[callStrings %~% as.character(getOption("reportrStackFilterIn"))[1]]
    if (!is.null(getOption("reportrStackFilterOut")))
        callStrings <- callStrings[callStrings %!~% as.character(getOption("reportrStackFilterOut"))[1]]
    
    return (callStrings)
}

.buildPrefix <- function (level, format = NULL)
{
    if (!is.null(format))
        prefix <- as.character(format)[1]
    else if (is.null(getOption("reportrPrefixFormat")))
        prefix <- "%d%L: "
    else
        prefix <- as.character(getOption("reportrPrefixFormat"))[1]
    
    if (prefix == "")
        return (prefix)
    else
    {
        if (prefix %~% "\\%(d|f)")
            stack <- .getCallStack()

        if (prefix %~% "\\%d")
            prefix <- gsub("%d", paste(rep("* ",length(stack)),collapse=""), prefix, fixed=TRUE)
        if (prefix %~% "\\%f")
            prefix <- gsub("%f", sub("^([\\w.]+)\\(.+$","\\1",stack[length(stack)],perl=TRUE), prefix, fixed=TRUE)
        if (prefix %~% "\\%l")
            prefix <- gsub("%l", tolower(names(OL)[which(OL==level)]), prefix, fixed=TRUE)
        if (prefix %~% "\\%L")
            prefix <- gsub("%L", toupper(names(OL)[which(OL==level)]), prefix, fixed=TRUE)
        if (prefix %~% "\\%p")
            prefix <- gsub("%p", Sys.getpid(), prefix, fixed=TRUE)

        return (prefix)
    }
}

.buildMessage <- function (...)
{
    message <- paste(..., sep="")
    keep <- TRUE
    
    if (!is.null(getOption("reportrMessageFilterIn")))
        keep <- keep & (message %~% as.character(getOption("reportrMessageFilterIn"))[1])
    if (!is.null(getOption("reportrMessageFilterOut")))
        keep <- keep & (message %!~% as.character(getOption("reportrMessageFilterOut"))[1])
    
    if (keep)
        return (message)
    else
        return (NULL)
}

ask <- function (..., default = NULL, prefixFormat = NULL)
{
    outputLevel <- getOutputLevel()
    message <- .buildMessage(...)
    if (outputLevel > OL$Question || is.null(message))
        return (default)
    else
    {
        reportFlags()
        ans <- readline(paste(.buildPrefix(OL$Question,prefixFormat), message, " ", sep=""))
        return (ans)
    }
}

report <- function (level, ..., prefixFormat = NULL)
{
    outputLevel <- getOutputLevel()
    message <- .buildMessage(...)
    if (outputLevel > level || is.null(message))
        return (invisible(NULL))
    
    reportFlags()
    cat(paste(.buildPrefix(level,prefixFormat), message, "\n", sep=""), file=stderr())
    
    if (outputLevel == OL$Debug)
    {
        stackTraceLevel <- getOption("reportrStackTraceLevel")
        if (is.null(stackTraceLevel))
            stackTraceLevel <- OL$Error
        if (level >= stackTraceLevel)
        {
            stack <- .getCallStack()
            cat("--- Begin stack trace ---\n", file=stderr())
            for (i in 1:length(stack))
                cat(rep("* ", i), stack[i], "\n", sep="", file=stderr())
            cat("---  End stack trace  ---\n", file=stderr())
        }
    }
    
    if (level == OL$Error)
        invokeRestart("abort")
}

flag <- function (level, ...)
{
    if (getOutputLevel() == OL$Debug)
    {
        stackTraceLevel <- getOption("reportrStackTraceLevel")
        if (is.null(stackTraceLevel))
            stackTraceLevel <- OL$Error
        if (level >= stackTraceLevel)
        {
            report(level, ...)
            return (invisible(NULL))
        }
    }
    
    message <- .buildMessage(...)
    if (is.null(message))
        return (invisible(NULL))
    currentFlag <- list(list(level=level, message=message))
    
    if (!exists("reportrFlags",.Workspace) || is.null(.Workspace$reportrFlags))
        .Workspace$reportrFlags <- currentFlag
    else
        .Workspace$reportrFlags <- c(.Workspace$reportrFlags, currentFlag)
}

reportFlags <- function ()
{
    if (exists("reportrFlags",.Workspace) && !is.null(.Workspace$reportrFlags))
    {
        levels <- unlist(lapply(.Workspace$reportrFlags, "[[", "level"))
        messages <- unlist(lapply(.Workspace$reportrFlags, "[[", "message"))
        
        # This is before the call to report() to avoid infinite recursion
        clearFlags()
        
        for (message in unique(messages))
        {
            locs <- which(messages == message)
            level <- max(levels[locs])
            if (length(locs) == 1)
                report(level, message, prefixFormat="%L: ")
            else
                report(level, paste("[x",length(locs),"] ",message,sep=""), prefixFormat="%L: ")
        }
    }
}

clearFlags <- function ()
{
    .Workspace$reportrFlags <- NULL
}
