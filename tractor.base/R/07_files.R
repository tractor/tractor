processFiles <- function (source, target = NULL, action = c("copy","move","symlink","delete"), overwrite = TRUE, relative = TRUE, all = FALSE)
{
    action <- match.arg(action)
    if (action == "symlink" && isTRUE(getOption("tractorNoSymlinks")))
        action <- "copy"
    
    sourceNames <- names(source)
    if (is.character(source))
    {
        sourceNames <- sourceNames %||% source
        source <- as.list(source)
    }
    else if (inherits(source, "concreteFileSet"))
        source <- list(source)
    assert(is.list(source), "Source must be a character vector, concrete file set or list")
    
    if (action != "delete")
    {
        dirTarget <- length(target) == 1L && dir.exists(target)
        assert(dirTarget || length(source) == length(target), "If the target isn't an existing directory it should match the source in length")
    }
    
    results <- structure(vector("list",length(source)), names=sourceNames)
    for (i in seq_along(source))
    {
        if (is.null(source[[i]]))
            next
        else if (inherits(source[[i]], "concreteFileSet"))
        {
            sourceFiles <- c(source[[i]]$requiredFiles, source[[i]]$auxiliaryFiles)
            if (all)
                sourceFiles <- c(sourceFiles, source[[i]]$otherFiles)
        }
        else
            sourceFiles <- source[[i]]
        
        if (action == "delete")
        {
            # The expand argument was added in R 4.0.0
            if (getRversion() >= "4.0")
                success <- unlink(sourceFiles, recursive=TRUE, expand=FALSE)
            else
                success <- unlink(sourceFiles, recursive=TRUE)
            results[[i]] <- (success == 0)
            next
        }
        
        if (dirTarget)
            targetFiles <- file.path(target, basename(sourceFiles))
        else
            targetFiles <- ensureFileSuffix(target[i], names(sourceFiles))
        
        for (parent in unique(dirname(targetFiles)))
            where(!dir.exists(parent), dir.create(parent,recursive=TRUE))
        
        noop <- sourceFiles == targetFiles
        if (all(noop))
            next
        sourceFiles <- sourceFiles[!noop]
        targetFiles <- targetFiles[!noop]
        
        if (action == "copy")
        {
            report(OL$Verbose, "Copying #{embrace(sourceFiles)} => #{embrace(targetFiles)}")
            success <- file.copy(sourceFiles, targetFiles, overwrite=overwrite)
        }
        else if (action == "move")
        {
            # Try file.rename() first, as it doesn't involve copying the data (but usually can't cross file systems)
            report(OL$Verbose, "Moving #{embrace(sourceFiles)} => #{embrace(targetFiles)}")
            success <- file.rename(sourceFiles, targetFiles)
            if (any(!success))
            {
                failed <- which(!success)
                success[failed] <- file.copy(sourceFiles[failed], targetFiles[failed], overwrite=overwrite)
                if (all(success))
                    unlink(sourceFiles[failed])
            }
        }
        else if (action == "symlink")
        {
            existingTargets <- !is.na(Sys.readlink(targetFiles))
            if (overwrite && any(existingTargets))
                unlink(targetFiles[existingTargets])
            else if (!overwrite && any(existingTargets))
            {
                report(OL$Verbose, "Existing targets #{embrace(targetFiles[existingTargets])} will not be overwritten")
                sourceFiles <- sourceFiles[!existingTargets]
                targetFiles <- targetFiles[!existingTargets]
            }
            if (relative)
            {
                for (j in seq_along(sourceFiles))
                    sourceFiles[j] <- relativePath(sourceFiles[j], targetFiles[j])
            }
            
            report(OL$Verbose, "Linking #{embrace(sourceFiles)} => #{embrace(targetFiles)}")
            success <- file.symlink(sourceFiles, targetFiles)
        }
        results[[i]] <- success
    }
    return (results)
}

#' The FileSet class
#' 
#' This reference class manages a set of related files based on a common file
#' stem and required or suffixes. It supports operations such as copying,
#' moving, symlinking, validating and deleting files, ensuring that all
#' constituent files are handled consistently. It is designed for handling
#' alternative file formats or composite file types.
#'
#' @field formats A named list mapping format names to required file suffixes.
#' @field validators An optional named list of validation functions for some
#'   or all of the supported formats.
#' @field auxiliaries A character vector of optional auxiliary file suffixes.
#' 
#' @export
FileSet <- setRefClass("FileSet", contains="TractorObject", fields=list(formats="list", validators="list", auxiliaries="character"), methods=list(
    atPaths = function (paths)
    {
        formats <- lapply(paths, function(path) .self$findFormat(path, all=TRUE))
        
        return (structure(list(
            formats = function () { return (formats) },
            
            copy = function (target, overwrite = TRUE) { processFiles(formats, target, action="copy", overwrite=overwrite) },
            
            delete = function () { processFiles(formats, action="delete", all=TRUE) },
            
            move = function (target, overwrite = TRUE) { processFiles(formats, target, action="move", overwrite=overwrite) },
            
            present = function () { return (!sapply(formats, is.null)) },
            
            symlink = function (target, overwrite = FALSE, relative = TRUE) { processFiles(formats, target, action="symlink", overwrite=overwrite, relative=relative) }
        ), fileSet=.self, class="fileSetHandle"))
    },
    
    fileStem = function (paths)
    {
        stem <- ensureFileSuffix(expandFileName(paths), NULL, strip=unlist(formats))
        return (ifelse(is.na(paths), NA_character_, stem))
    },
    
    findFormat = function (paths, intent = c("read","write"), all = FALSE)
    {
        if (length(paths) > 1L)
            return (setNames(lapply(paths, .self$findFormat), paths))
        if (is.na(paths))
            return (NULL)
        
        stem <- .self$fileStem(paths)
        intent <- match.arg(intent)
        auxPaths <- setNames(ensureFileSuffix(stem, auxiliaries), auxiliaries)
        if (intent == "read")
            auxPaths <- auxPaths[file.exists(auxPaths)]
        
        result <- NULL
        for (formatName in names(formats))
        {
            suffixes <- formats[[formatName]]
            realPaths <- ensureFileSuffix(stem, suffixes)
            if (intent == "write" || all(file.exists(realPaths)))
            {
                validator <- validators[[formatName]]
                if (intent == "read" && !is.null(validator) && !isTRUE(try(validator(setNames(realPaths, suffixes)), silent=TRUE)))
                    next
                
                if (is.null(result))
                {
                    result <- list(format=formatName, stem=stem, requiredFiles=setNames(realPaths,suffixes), auxiliaryFiles=auxPaths)
                    if (!all) break
                }
                else
                {
                    result$otherFormats <- c(result$otherFormats, formatName)
                    result$otherFiles <- c(result$otherFiles, realPaths)
                }
            }
        }
        return (where(!is.null(result), structure(result, class="concreteFileSet")))
    },
    
    subset = function (match, ...)
    {
        regex <- where(is_ore(match), match, ore(match, options="i", syntax="fixed"))
        subformats <- formats[names(formats) %~% regex]
        subvalidators <- validators[names(validators) %~% regex]
        return (.self$getRefClass()$new(formats=subformats, validators=subvalidators, ...))
    }
))

#' @export
FileMap <- setRefClass("FileMap", contains="TractorObject", fields=list(directory="character", map="list"), methods=list(
    initialize = function (path = "", ...)
    {
        dir <- expandFileName(ifelse(dir.exists(path), path, dirname(path)))
        object <- initFields(directory=dir, map=list())
        object$read()
        return (object)
    },
    
    dropElement = function (key) { .self$setElement(key, NULL) },
    
    getDirectory = function () { return (directory) },
    
    getElement = function (key) { return (map[[key]]) },
    
    getFile = function () { return (file.path(directory, "map.yaml")) },
    
    getMap = function (sorted = FALSE) { where(sorted, map[sort(names(map))], map) },
    
    setElement = function (key, value)
    {
        .self$map[[key]] <- value
        invisible (.self)
    },
    
    read = function ()
    {
        file <- .self$getFile()
        if (directory != "" && file.exists(file))
            .self$map <- yaml::read_yaml(mapFile)
        invisible (.self)
    },
    
    write = function ()
    {
        file <- .self$getFile()
        if (directory != "")
        {
            if (length(map) > 0L)
            {
                if (!dir.exists(directory))
                    dir.create(directory, recursive=TRUE)
                yaml::write_yaml(map, file)
            }
            else if (file.exists(file))
                unlink(file)
        }
        invisible (.self)
    }
))

#' @export
setMethod("[", signature(x="FileMap",i="character",j="missing"), function (x, i, j, ..., drop = TRUE) {
    x$getElement(i)
})

#' @export
setReplaceMethod("[", signature(x="FileMap",i="character",j="missing"), function (x, i, j, ..., value) {
    x$setElement(i, value)
})

#' @export
setMethod("==", signature(e1="FileMap",e2="FileMap"), function (e1, e2) {
    isTRUE(all.equal(e1$getMap(TRUE), e2$getMap(TRUE)))
})

#' @export
setMethod("!=", signature(e1="FileMap",e2="FileMap"), function (e1, e2) {
    !isTRUE(all.equal(e1$getMap(TRUE), e2$getMap(TRUE)))
})

niftiVersionCheck <- function (versions)
{
    return (function(x) all(RNifti::niftiVersion(x) %in% versions))
}

.imageFormats <- list(
    nifti="nii",
    nifti_gz="nii.gz",
    nifti_pair=c("hdr","img"),
    nifti_pair_gz=c("hdr.gz","img.gz"),
    mrtrix="mif",
    mrtrix_gz="mif.gz",
    mgh="mgh",
    mgh_gz="mgz",
    analyze=c("hdr","img"),
    analyze_gz=c("hdr.gz","img.gz"))

.imageValidators <- list(
    nifti=niftiVersionCheck(1:2),
    nifti_gz=niftiVersionCheck(1:2),
    nifti_pair=niftiVersionCheck(1:2),
    nifti_pair_gz=niftiVersionCheck(1:2),
    analyze=niftiVersionCheck(0),
    analyze_gz=niftiVersionCheck(0))

#' @export
ImageFileSet <- setRefClass("ImageFileSet", contains="FileSet", fields=list(defaultMap="list"), methods=list(
    initialize = function (formats = .imageFormats, validators = .imageValidators, auxiliaries = c("dirs","lut","tags"), defaultMap = list(), ...)
    {
        object <- callSuper(formats=formats, validators=validators, auxiliaries=auxiliaries)
        object$defaultMap <- defaultMap
        return (object)
    },
    
    atPath = function (path)
    {
        .super <- callSuper(path)
        map <- FileMap$new(path)
        
        result <- .super
        result$delete <- function ()
        {
            # Deleting a mapped image just unmaps it
            if (.super$present())
                .super$delete()
            else
                map$dropElement(basename(.self$fileStem(path)))$write()
        }
        result$map <- function (target, overwrite = TRUE, relative = TRUE)
        {
            if (is.null(.self$findFormat(target)))
                .super$move(target)
            else if (!is.null(.self$findFormat(path)))
            {
                if (overwrite)
                    .super$delete()
                else
                {
                    report(OL$Verbose, "Existing image path #{path} will not be replaced by a mapping")
                    return (FALSE)
                }
            }
            target <- .self$fileStem(target)
            value <- where(relative, relativePath(target,path), target)
            map$setElement(basename(.self$fileStem(path)), value)$write()
            return (TRUE)
        }
        return (result)
    },
    
    findFormat = function (paths, intent = c("read","write"), all = FALSE)
    {
        if (length(paths) > 1L)
            return (setNames(lapply(paths, .self$findFormat), paths))
        if (is.na(paths))
            return (NULL)
        
        stem <- .self$fileStem(paths)
        intent <- match.arg(intent)
        
        # If the stem matches literally then set header and image fields and return
        result <- callSuper(stem, intent=intent, all=all)
        if (!is.null(result))
        {
            if (length(result$requiredFiles) == 1L)
                result$headerFile <- result$imageFile <- result$requiredFiles
            else
            {
                result$headerFile <- result$requiredFiles[names(result$requiredFiles) %~|% "^hdr"]
                result$imageFile <- result$requiredFiles[names(result$requiredFiles) %~|% "^img"]
            }
            return (result)
        }
        
        mapValue <- FileMap$new(stem)$getElement(basename(stem))
        if (!is.null(mapValue))
            return (findFormat(file.path(dirname(stem), mapValue)))
        else
        {
            value <- defaultMap[[basename(stem)]]
            return (where(!is.null(value), findFormat(file.path(dirname(stem), value))))
        }
    }
))

.ImageFiles <- ImageFileSet$new()

#' @export
imageFiles <- function (paths = NULL)
{
    if (is.null(paths))
        return (.ImageFiles)
    else
        return (.ImageFiles$atPaths(paths))
}
