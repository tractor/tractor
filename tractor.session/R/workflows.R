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

precheckWorkflow <- function (file, session, commandOnly = FALSE)
{
    if (!file.exists(file))
        file <- findWorkflow(file)
    
    result <- list(ok=TRUE, problems=NULL, commandPath=NULL)
    logProblem <- function (p)
    {
        result$ok <<- FALSE
        result$problems <<- c(result$problems, es(p, envir=parent.frame()))
    }
    
    lines <- readLines(file)
    
    commandLines <- lines %~|% "^\\s*#@command\\s+([\\w\\-,\\s]+)$"
    if (length(commandLines) == 0)
        logProblem("Workflow file #{file} contains no command directive")
    
    commands <- ore.split("[,\\s]+", groups(ore.search("^\\s*#@command\\s+([\\w\\-,\\s]+)$",commandLines)))
    for (command in commands)
    {
        command <- locateExecutable(command, errorIfMissing=FALSE)
        if (!is.null(command))
        {
            result$commandPath <- command
            break
        }
    }
    if (is.null(result$commandPath))
        logProblem("No suitable command (#{implode(commands,', ')}) can be found for workflow #{file}")
    
    if (!commandOnly)
    {
        prereqs <- ore.split("[,\\s]+", groups(ore.search("^\\s*#@prereq\\s+(.+)$",lines)))
        for (prereq in prereqs)
        {
            prereqPath <- resolvePath(prereq, defaultSessionPath=as.character(session))
            if (!file.exists(prereqPath) && !imageFileExists(prereqPath))
                logProblem("Prerequisite file #{prereqPath} is missing for workflow #{file}")
        }
    }
    
    return (result)
}

runWorkflow <- function (name, session, ...)
{
    workflowFile <- findWorkflow(name)
    
    env <- list(...)
    env <- structure(as.character(env), names=names(env))
    
    directory <- as.character(session)
    if (length(directory) != 1)
        report(OL$Error, "The session directory must be a single string")
    
    check <- precheckWorkflow(workflowFile, directory)
    assert(check$ok, check$problems[1])
    
    tractorFlags <- na.omit(matches(ore.search("-[dziva](\\s+[^-]\\S*)?", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    furrowFlags <- na.omit(matches(ore.search("-z", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    
    tractorPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "tractor")
    furrowPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "furrow")
    
    sysenv <- Sys.getenv()
    controlenv <- c(TRACTOR_COMMAND=check$commandPath, TRACTOR_SESSION_PATH=directory, TRACTOR_WORKING_DIR=directory, TRACTOR=es("#{tractorPath} -q #{implode(tractorFlags,' ')}"), FURROW=es("#{furrowPath} #{implode(furrowFlags,' ')}"), TRACTOR_FLAGS="", PS4="\x1b[32m==> \x1b[0m")
    env <- deduplicate(c(env, controlenv, sysenv[names(sysenv) %~|% "^TRACTOR_"]))
    env[env %~% "\\s"] <- es("\"#{env[env %~% '\\\\s']}\"")
    env <- paste(names(env), env, sep="=")
    report(OL$Debug, "Environment: #{implode(env,', ')}")
    
    report(OL$Verbose, "Running workflow \"#{name}\"...")
    startTime <- Sys.time()
    
    # If the workflow file is executable, run it directly; otherwise call bash
    if (file.access(workflowFile, 1L) == 0L)
        returnValue <- execute(workflowFile, env=env)
    else
        returnValue <- execute("bash", c("-e",workflowFile), env=env)
    
    if (returnValue != 0)
        report(OL$Error, "Workflow \"#{name}\" failed with error code #{returnValue}")
    else
    {
        runTime <- Sys.time() - startTime
        report(OL$Verbose, "Workflow completed in #{runTime} #{units(runTime)}", round=2)
    }
    
    invisible (returnValue)
}
