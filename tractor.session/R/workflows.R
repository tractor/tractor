findWorkflow <- function (name)
{
    workflowFile <- ensureFileSuffix(name, "sh")
    pathDirs <- c(".",
                  file.path(Sys.getenv("HOME"), ".tractor"),
                  splitAndConvertString(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE),
                  file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "workflows"))
    possibleLocations <- file.path(pathDirs, workflowFile)
    filesExist <- file.exists(possibleLocations)
    
    if (sum(filesExist) == 0)
        report(OL$Error, "Workflow \"#{workflowFile}\" not found")
    else
    {
        realLocations <- possibleLocations[filesExist]
        return (realLocations[1])
    }
}

runWorkflow <- function (name, session, ...)
{
    workflowFile <- findWorkflow(name)
    
    env <- list(...)
    env <- structure(as.character(env), names=names(env))
    
    if (is(session, "MriSession"))
        directory <- session$getDirectory()
    else
        directory <- as.character(session)
    if (length(directory) != 1)
        report(OL$Error, "The session directory must be a single string")
    
    commandLines <- readLines(workflowFile) %~|% "^\\s*#@command\\s+([\\w\\-,\\s]+)$"
    if (length(commandLines) == 0)
        report(OL$Error, "Workflow file #{workflowFile} contains no command directive")
    
    commands <- ore.split("[,\\s]+", groups(ore.search("^\\s*#@command\\s+([\\w\\-,\\s]+)$",commandLines)))
    commandPath <- NULL
    for (command in commands)
    {
        command <- locateExecutable(command, errorIfMissing=FALSE)
        if (!is.null(command))
        {
            commandPath <- command
            break
        }
    }
    if (is.null(commandPath))
        report(OL$Error, "No suitable command (#{implode(commands,', ')}) can be found for workflow #{workflowFile}")
    
    tractorFlags <- na.omit(matches(ore.search("-[dziva](\\s+[^-]\\S*)?", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    furrowFlags <- na.omit(matches(ore.search("-z", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    
    tractorPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "tractor")
    furrowPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "furrow")
    
    sysenv <- Sys.getenv()
    controlenv <- c(TRACTOR_COMMAND=commandPath, TRACTOR_SESSION_PATH=directory, TRACTOR=es("\"#{tractorPath} -w #{directory} -q #{implode(tractorFlags,' ')}\""), FURROW=es("\"#{furrowPath} -w #{directory} #{implode(furrowFlags,' ')}\""), TRACTOR_FLAGS="", PS4="\"\x1b[32m==> \x1b[0m\"")
    env <- deduplicate(c(env, controlenv, sysenv[names(sysenv) %~|% "^TRACTOR_"]))
    env <- paste(names(env), env, sep="=")
    
    report(OL$Verbose, "Running workflow \"#{name}\"...")
    
    # If the workflow file is executable, run it directly; otherwise call bash
    if (file.access(workflowFile, 1L) == 0L)
        returnValue <- execute(workflowFile, env=env)
    else
        returnValue <- execute("bash", c("-e",workflowFile), env=env)
    
    if (returnValue != 0)
        report(OL$Error, "Workflow \"#{name}\" failed with error code #{returnValue}")
    
    invisible (returnValue)
}
