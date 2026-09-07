#' Run a TractoR experiment repeatedly over a set of parameter combinations
#'
#' This function underlies the `plough` shell command, which runs a
#' `tractor` experiment script multiple times, either varying one or more
#' configuration variables read from YAML file(s), or simply repeating the
#' same configuration a fixed number of times. Runs may be dispatched to a
#' Sun/Oracle Grid Engine cluster via `qsub`, run in parallel across local
#' cores (see [parallelApply()]), or run serially.
#'
#' @param scriptName A string giving the name of the `tractor` experiment
#'   script to run.
#' @param configFiles A character vector, or colon-separated string, of
#'   paths to YAML configuration files (see [readYaml()]).
#' @param variables A comma-separated string naming the configuration
#'   variables to loop over. If empty, all variables with more than one
#'   value are used if `crossApply` is set, otherwise all singly-valued
#'   variables are used. Ignored if `repetitions` is greater than zero.
#' @param tractorFlags A string of flags to pass to the underlying
#'   `tractor` calls. Occurrences of `%name` are substituted with the
#'   current value of configuration variable `name`, and `%%` with the
#'   (1-based) iteration index.
#' @param tractorOptions As for `tractorFlags`, but for experiment-specific
#'   options placed after the script name rather than command flags.
#' @param useGridEngine Boolean-like value (compared to `1`): if true,
#'   jobs are submitted as a Grid Engine array job via `qsub` rather than
#'   run locally.
#' @param crossApply Boolean-like value (compared to `1`): if true, every
#'   combination of the looped variables' values is used (via
#'   `expand.grid()`); otherwise corresponding elements of each variable
#'   are used together, recycling shorter vectors as needed.
#' @param queueName A string giving the Grid Engine queue name to submit
#'   to, or an empty string to use the default queue. Only relevant if
#'   `useGridEngine` is true.
#' @param qsubOptions A string of additional options to pass to `qsub`.
#'   Only relevant if `useGridEngine` is true.
#' @param parallelisationFactor An integer giving the number of local
#'   cores to parallelise over. Ignored if `useGridEngine` is true.
#' @param debug Boolean-like value (compared to `1`): if true, the
#'   `reportr` output level is set to `OL$Debug` rather than `OL$Info`.
#' @param repetitions An integer giving the number of times to repeat the
#'   experiment using the same, unmodified configuration. If greater than
#'   zero, this takes priority over `variables` and `crossApply`.
#' @return Called for its side effect of scheduling or running the
#'   requested jobs. `NULL` is returned invisibly.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
ploughExperiment <- function (scriptName, configFiles, variables, tractorFlags, tractorOptions, useGridEngine, crossApply, queueName, qsubOptions, parallelisationFactor, debug, repetitions)
{
    crossApply <- isTRUE(crossApply == 1)
    useGridEngine <- isTRUE(useGridEngine == 1)
    setOutputLevel(ifelse(isTRUE(debug==1), OL$Debug, OL$Info))
    
    if (isValidAs(repetitions, "integer"))
        repetitions <- as.integer(repetitions)
    else
        repetitions <- 0L
    
    config <- readYaml(configFiles)
    variableLengths <- sapply(config, length)
    
    variables <- splitAndConvertString(variables, ",", fixed=TRUE)
    variables <- variables[variables != ""]
    if (repetitions > 0)
        variables <- character(0)
    else if (length(variables) == 0)
    {
        if (any(variableLengths > 1))
            variables <- names(config)[variableLengths > 1]
        else
            variables <- names(config)[variableLengths == 1]
    }
    else if (!all(variables %in% names(config)))
        report(OL$Error, "Specified variable(s) #{implode(variables[!(variables %in% names(config))],sep=', ',finalSep=' and ')} are not mentioned in the config files")
    
    if (length(variables) > 0)
        report(OL$Info, "Looping over variable(s) #{implode(variables,sep=', ',finalSep=' and ')}")
    
    usingParallel <- FALSE
    if (isValidAs(parallelisationFactor,"integer") && as.integer(parallelisationFactor) > 1)
    {
        if (useGridEngine)
            report(OL$Warning, "Parallelisation factor will be ignored when using the grid engine")
        else if (system.file(package="parallel") != "")
        {
            library(parallel)
            options(mc.cores=as.integer(parallelisationFactor))
            usingParallel <- TRUE
        }
        else
            report(OL$Warning, "The \"parallel\" package is not installed - code will not be parallelised")
    }
    
    data <- configFile <- NULL
    if (repetitions > 0)
    {
        n <- as.integer(repetitions)
        data <- as.data.frame(config, stringsAsFactors=FALSE)
        configFile <- ensureFileSuffix(threadSafeTempFile(), "yaml")
        writeYaml(config, configFile)
    }
    else
    {
        if (crossApply)
        {
            n <- prod(variableLengths[variables])
            data <- as.data.frame(expand.grid(config[variables], stringsAsFactors=FALSE), stringsAsFactors=FALSE)
        }
        else
        {
            n <- max(variableLengths[variables])
            data <- as.data.frame(lapply(config[variables], rep, length.out=n), stringsAsFactors=FALSE)
        }
    
        for (name in setdiff(names(config), c(variables,"")))
            data[[name]] <- rep(config[[name]], length.out=n)
    }
    
    report(OL$Info, "Scheduling #{n} jobs")
    
    tractorPath <- file.path(Sys.getenv("TRACTOR_HOME"), "bin", "tractor")
    
    buildArgs <- function (i)
    {
        index <- ifelse(repetitions > 0, 1, i)
        currentFlags <- ore.subst("(?<!\\\\)\\%([A-Za-z]+)", function(match) data[index,groups(match)], tractorFlags, all=TRUE)
        currentFlags <- ore.subst("(?<!\\\\)\\%\\%", as.character(i), currentFlags, all=TRUE)
        currentOptions <- ore.subst("(?<!\\\\)\\%([A-Za-z]+)", function(match) data[index,groups(match)], tractorOptions, all=TRUE)
        currentOptions <- ore.subst("(?<!\\\\)\\%\\%", as.character(i), currentOptions, all=TRUE)
        return (es("#{currentFlags} #{scriptName} #{currentOptions}"))
    }
    
    if (useGridEngine)
    {
        tempDir <- expandFileName(paste("getmp", implode(sample(c(0:9,letters[1:6]),6),""), sep="_"))
        if (file.exists(tempDir))
            unlink(tempDir, recursive=TRUE)
        dir.create(file.path(tempDir,"log"), recursive=TRUE)
        
        args <- sapply(seq_len(n), buildArgs)
        argsFile <- file.path(tempDir, "args")
        writeLines(args, argsFile)
        
        configPrefix <- file.path(tempDir, "config")
        if (repetitions > 0)
            file.copy(configFile, es("#{configPrefix}.yaml"))
        else
        {
            for (i in seq_len(n))
                writeYaml(as.list(data[i,,drop=FALSE]), es("#{configPrefix}.#{i}.yaml"))
        }
        
        qsubScriptFile <- file.path(tempDir, "script")
        qsubScript <- c("#!/bin/sh",
                        "#$ -S /bin/bash",
                        es("export TRACTOR_PLOUGH_ID=${SGE_TASK_ID}"),
                        es("TRACTOR_ARGS=`sed \"${SGE_TASK_ID}q;d\" #{argsFile}`"),
                        ifelse(repetitions > 0,
                            es("#{tractorPath} -c #{configPrefix}.yaml ${TRACTOR_ARGS}"),
                            es("#{tractorPath} -c #{configPrefix}.${SGE_TASK_ID}.yaml ${TRACTOR_ARGS}")))
        writeLines(qsubScript, qsubScriptFile)
        execute("chmod", es("+x #{qsubScriptFile}"))
        
        queueOption <- ifelse(queueName=="", "", es("-q #{queueName}"))
        qsubArgs <- es("-terse -V -wd #{path.expand(getwd())} #{queueOption} -N #{scriptName} -o #{file.path(tempDir,'log')} -e /dev/null -t 1-#{n} #{qsubOptions} #{qsubScriptFile}")
        result <- execute("qsub", qsubArgs, stdout=TRUE)
        jobNumber <- as.numeric(ore.search("^(\\d+)\\.?.*$", result)[,1])
        jobNumber <- jobNumber[!is.na(jobNumber)]
        report(OL$Info, "Job number is #{jobNumber}")
    }
    else
    {
        parallelApply(seq_len(n), function(i) {
            if (repetitions > 0)
                currentFile <- configFile
            else
            {
                currentFile <- threadSafeTempFile()
                writeYaml(as.list(data[i,,drop=FALSE]), currentFile)
                on.exit(unlink(currentFile))
            }
            
            # FIXME: Using callExperiment() would be preferable, to allow interrupts, but flags will need to be parsed
            # callExperiment(exptName, args = NULL, configFiles = NULL, outputLevel = getOutputLevel(), ...)
            execute(tractorPath, es("-c #{currentFile} #{buildArgs(i)}"), env=es("TRACTOR_PLOUGH_ID=#{i}"))
        })
    }
    
    invisible(NULL)
}
