#@args mode, output directory, [package names]
#@desc Measure combined test coverage of TractoR's core packages and experiment scripts. The "instrument" mode installs the named core packages (by default all of them) into a "lib" subdirectory of the output directory, with hooks added in the style of covr::package_coverage(), and writes a suitable value for the TRACTOR_LIBRARY environment variable to a "library" file there. Any TractoR process subsequently run with that library path and COVERAGE_DIR set to the "traces" subdirectory will record its coverage on exit. The "report" mode then merges the traces, prints a summary, and writes per-file results, the merged coverage object and a JSON file in Codecov's format into the output directory. Unlike package_coverage(), this works for several packages at once, because covr's own hooks reset the trace counters each time an instrumented package is loaded. The "covr" package must be installed.
#@nohistory TRUE

runExperiment <- function ()
{
    requireArguments("mode", "output directory")
    mode <- match.arg(Arguments[1], c("instrument","report"))
    outDir <- expandFileName(Arguments[2])
    root <- normalizePath(Sys.getenv("TRACTOR_HOME"))

    if (!requireNamespace("covr", quietly=TRUE))
        report(OL$Error, "The \"covr\" package is required for coverage testing")

    if (mode == "instrument")
    {
        packages <- Arguments[-(1:2)]
        if (length(packages) == 0)
            packages <- basename(dirname(Sys.glob(file.path(root, "tractor.*", "DESCRIPTION"))))
        if (!("tractor.base" %in% packages))
            report(OL$Error, "The \"tractor.base\" package must be instrumented")

        lib <- file.path(outDir, "lib")
        dir.create(lib, recursive=TRUE, showWarnings=FALSE)
        dir.create(file.path(outDir,"traces"), showWarnings=FALSE)

        # Trace a package or experiment environment without clearing existing counters
        traceCode <- "{ .counters <- as.list(covr:::.counters); covr:::trace_environment(%s); list2env(.counters, covr:::.counters) }"
        traceDir <- 'Sys.getenv("COVERAGE_DIR")'

        for (name in packages)
        {
            cat(paste0("Installing instrumented package \"", name, "\"... "))
            # A failed installation only produces a warning, so promote it
            tryCatch(utils::install.packages(file.path(root,name), lib=lib, repos=NULL, type="source", quiet=TRUE, INSTALL_opts=c("--with-keep.source","--with-keep.parse.data","--no-multiarch")),
                warning=function(w) report(OL$Error, "Installation of package \"", name, "\" failed: ", conditionMessage(w)))

            hooks <- sprintf('setHook(packageEvent(pkg, "onLoad"), function(...) %s)', sprintf(traceCode,"ns"))

            # Process-wide hooks are added to tractor.base only, since it is
            # always loaded: these save the traces once on exit (or exit from a
            # forked child), and trace experiment scripts after
            # bootstrapExperiment() sources them
            if (name == "tractor.base")
            {
                hooks <- c(hooks,
                    sprintf('reg.finalizer(ns, function(...) if (is.null(getOption("tractor.coverageSaved"))) { options(tractor.coverageSaved=TRUE); covr:::save_trace(%s) }, onexit=TRUE)', traceDir),
                    sprintf('covr:::fix_mcexit(\'%s\')', traceDir),
                    sprintf('setHook("tractor.experimentSourced", function(env, file) %s)', sprintf(traceCode,"env")))
            }

            loader <- file.path(lib, name, "R", name)
            lines <- readLines(loader)
            writeLines(append(lines, hooks, length(lines)-1L), loader)
            cat("OK\n")
        }

        # The library path needs to include covr and its dependencies, wherever they are
        loadNamespace("covr")
        libs <- dirname(vapply(loadedNamespaces(), find.package, "", quiet=TRUE))
        libs <- setdiff(libs[libs != "."], .Library)
        tractorLibs <- strsplit(Sys.getenv("TRACTOR_LIBRARY"), .Platform$path.sep, fixed=TRUE)[[1]]
        writeLines(paste(unique(c(lib, tractorLibs, libs)), collapse=.Platform$path.sep), file.path(outDir,"library"))
    }
    else
    {
        traceFiles <- list.files(file.path(outDir,"traces"), "^covr_trace_", full.names=TRUE)
        if (length(traceFiles) == 0)
            report(OL$Error, "No coverage traces found in ", file.path(outDir,"traces"))

        # Merge the traces (much faster than covr:::merge_coverage for many files)
        traces <- unlist(lapply(traceFiles, function(f) as.list(readRDS(f))), recursive=FALSE)
        totals <- tapply(vapply(traces, function(x) as.numeric(x$value), 0), names(traces), sum)
        counters <- traces[!duplicated(names(traces))]
        for (key in names(counters))
            counters[[key]]$value <- totals[[key]]

        # Experiment scripts that were never run have no counters, so we trace
        # their functions here, without running them, to count them as uncovered
        # (normalisation below makes sure that the file name check is valid)
        sourceFile <- function (x)
        {
            srcfile <- attr(x$srcref, "srcfile")
            path <- srcfile$filename
            if (!grepl("^/", path) && !is.null(srcfile$wd))
                path <- file.path(srcfile$wd, path)
            return (normalizePath(path, mustWork=FALSE))
        }
        scripts <- normalizePath(list.files(file.path(root,"share","tractor","experiments"), "\\.R$", full.names=TRUE))
        for (script in setdiff(scripts, vapply(counters, sourceFile, "")))
        {
            # Only function definitions are evaluated, to avoid side-effects
            env <- new.env()
            for (expr in parse(script, keep.source=TRUE))
            {
                if (is.call(expr) && as.character(expr[[1]]) %in% c("<-","=") && is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function")))
                    eval(expr, env)
            }
            covr:::trace_environment(env)
            counters <- c(counters, as.list(covr:::.counters))
        }

        # Normalise file names, which may contain relative elements, and drop
        # sources outside the core packages and standard experiment scripts
        files <- vapply(counters, sourceFile, "")
        for (i in seq_along(counters))
            assign("filename", files[i], envir=attr(counters[[i]]$srcref,"srcfile"))
        relativeFiles <- ifelse(startsWith(files, paste0(root,"/")), substring(files, nchar(root)+2L), "")
        counters <- counters[relativeFiles %~% "^(tractor\\.\\w+/R|share/tractor/experiments)/"]

        coverage <- covr:::as_coverage(counters, package=list(path=root, package="tractor"), root=root)
        saveRDS(coverage, file.path(outDir,"coverage.rds"))

        # Line-level hit counts
        lines <- covr::tally_coverage(coverage, by="line")
        lines$filename <- sub(paste0("^",root,"/"), "", lines$filename, fixed=FALSE)
        lines <- lines[!duplicated(lines[c("filename","line")]),]
        lines$covered <- lines$value > 0
        lines$component <- ifelse(startsWith(lines$filename,"share/"), "experiments", sub("/.*$", "", lines$filename))

        summarise <- function (groups)
        {
            result <- data.frame(lines=as.vector(table(groups)), covered=as.vector(tapply(lines$covered, groups, sum)), row.names=sort(unique(groups)))
            result$percent <- round(100 * result$covered / result$lines, 1)
            return (result)
        }

        byComponent <- summarise(lines$component)
        byFile <- summarise(lines$filename)
        byFile <- byFile[order(byFile$percent, rownames(byFile)),]

        cat(paste0("Combined coverage from ", length(traceFiles), " R processes:\n\n"))
        print(rbind(byComponent, total=data.frame(lines=nrow(lines), covered=sum(lines$covered), percent=round(100*mean(lines$covered),1))))

        sink(file.path(outDir, "coverage.txt"))
        print(byFile)
        sink()

        # Codecov's own JSON format: {"coverage": {"<path>": {"<line>": <hits>, ...}, ...}}
        json <- lapply(split(lines, lines$filename), function(x) {
            x <- x[order(x$line),]
            as.list(structure(x$value, names=x$line))
        })
        writeLines(jsonlite::toJSON(list(coverage=json), auto_unbox=TRUE, digits=NA), file.path(outDir,"codecov.json"))

        cat(paste0("\nPer-file results written to ", file.path(outDir,"coverage.txt"), "\n"))
    }
}
