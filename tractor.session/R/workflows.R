#' Locate a TractoR workflow script
#'
#' This function locates a TractoR "workflow" shell script by name, searching
#' the current directory, the user's `~/.tractor` directory, the directories
#' named in the `TRACTOR_PATH` environment variable, the `tractor/workflows`
#' directories of any packages named in `TRACTOR_PACKAGES`, and finally
#' TractoR's own standard workflow directory (`share/tractor/workflows`), in
#' that order.
#'
#' @param name A string giving the name of the workflow, with or without a
#'   `.sh` suffix.
#' @return The path to the first matching workflow script found.
#' @seealso [runWorkflow()], which runs a workflow located in this way.
#' @author Jon Clayden
#' @export
findWorkflow <- function (name)
{
    packagePaths <- unlist(lapply(splitAndConvertString(Sys.getenv("TRACTOR_PACKAGES"), "[:,]"), function(p) system.file("tractor", "workflows", package=p)))
    
    workflowFile <- ensureFileSuffix(name, "sh")
    pathDirs <- c(".",
                  file.path(Sys.getenv("HOME"), ".tractor"),
                  splitAndConvertString(Sys.getenv("TRACTOR_PATH"), ":", fixed=TRUE),
                  packagePaths[packagePaths != ""],
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

#' Check that a workflow's prerequisites are met
#'
#' This function checks whether a TractoR workflow script's stated
#' requirements are satisfied: that a suitable external command is available
#' on the system path (as declared by a `#@command` directive within the
#' script), and, unless `commandOnly` is `TRUE`, that any prerequisite files
#' it declares (via `#@prereq` directives) exist for the given session.
#'
#' @param file A string giving the path to a workflow script, or the name of
#'   a workflow to locate with [findWorkflow()].
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory, used to resolve relative prerequisite file paths.
#' @param commandOnly Boolean value: if `TRUE`, only the availability of a
#'   suitable command is checked, and prerequisite files are ignored.
#' @return A list with elements `ok` (a boolean value indicating whether all
#'   checks passed), `problems` (a character vector describing any problems
#'   found) and `commandPath` (the path to the located command, or `NULL`).
#' @seealso [runWorkflow()], which calls this function before running a
#'   workflow.
#' @author Jon Clayden
#' @export
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

#' Run a TractoR workflow
#'
#' This function runs a TractoR "workflow": a shell script, usually wrapping
#' one or more calls to third-party command-line tools such as FSL or
#' FreeSurfer programs, which together implement a step of a typical
#' tractography or other MRI processing pipeline. The workflow's location is
#' found using [findWorkflow()] and its prerequisites checked using
#' [precheckWorkflow()] before it is run, with a suitable environment
#' including session-specific and other configuration variables.
#'
#' @param name A string giving the name of the workflow to run (see
#'   [findWorkflow()]).
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory, on which the workflow will operate.
#' @param ... Named configuration variables to make available to the
#'   workflow script as environment variables.
#' @param .args A string, or character vector, of additional command-line
#'   arguments to make available to the workflow script (as
#'   `TRACTOR_COMMAND_ARGS`).
#' @return The workflow's exit code, invisibly. An error is raised if this is
#'   nonzero.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runWorkflow <- function (name, session, ..., .args = "")
{
    workflowFile <- findWorkflow(name)
    
    env <- list(...)
    if (length(env) > 0L)
    {
        env <- env[!sapply(env, is.null)]
        env <- structure(as.character(env), names=names(env))
    }
    
    directory <- as.character(session)
    if (length(directory) != 1)
        report(OL$Error, "The session directory must be a single string")
    
    check <- precheckWorkflow(workflowFile, directory)
    assert(check$ok, check$problems[1])
    
    tractorFlags <- na.omit(matches(ore.search("-[dziva](\\s+[^-]\\S*)?", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    furrowFlags <- na.omit(matches(ore.search("-z", Sys.getenv("TRACTOR_FLAGS"), all=TRUE)))
    
    path <- paste(file.path(Sys.getenv("TRACTOR_HOME"),"bin"), Sys.getenv("PATH"), sep=":")
    
    sysenv <- Sys.getenv()
    controlenv <- c(PATH=path, TRACTOR_COMMAND=check$commandPath, TRACTOR_COMMAND_ARGS=implode(.args," "), TRACTOR_SESSION_PATH=directory, TRACTOR_WORKING_DIR=directory, TRACTOR=es("tractor -q #{implode(tractorFlags,' ')}"), FURROW=es("furrow #{implode(furrowFlags,' ')}"), TRACTOR_FLAGS="", PS4="\x1b[32m==> \x1b[0m")
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
