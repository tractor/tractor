processFiles <- function (fileSet, stems, target = NULL, action = c("copy","move","symlink","delete"), overwrite = TRUE, relative = TRUE, all = FALSE)
{
    action <- match.arg(action)
    if (action == "symlink" && isTRUE(getOption("tractorNoSymlinks")))
        action <- "copy"
    results <- list()
    
    if (action != "delete")
    {
        dirTarget <- length(target) == 1L && dir.exists(target)
        assert(dirTarget || length(stems) == length(target), "If the target isn't an existing directory it should match the source in length")
    }
    
    for (i in seq_along(stems))
    {
        info <- fileSet$findFormat(stems[i], all=all)
        if (is.null(info))
        {
            report(OL$Warning, "No valid format found for file stem #{stems[i]}")
            next
        }
        
        sourceFiles <- c(info$requiredFiles, info$auxiliaryFiles, info$otherFiles)
        
        if (action == "delete")
        {
            # The expand argument was added in R 4.0.0
            if (getRversion() >= "4.0")
                success <- unlink(sourceFiles, recursive=TRUE, expand=FALSE)
            else
                success <- unlink(sourceFiles, recursive=TRUE)
            results[[stems[i]]] <- (success == 0)
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
        results[[stems[i]]] <- success
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
    atPath = function (path)
    {
        return (structure(list(
            copy = function (target, overwrite = TRUE) { processFiles(.self, path, target, action="copy") },
            
            delete = function () { processFiles(.self, path, action="delete", all=TRUE) },
            
            move = function (target, overwrite = TRUE) { processFiles(.self, path, target, action="move") },
            
            present = function ()
            {
                if (length(path) == 1L)
                    return (!is.null(.self$findFormat(path)))
                else
                    return (!sapply(.self$findFormat(path), is.null))
            },
            
            symlink = function (target, overwrite = FALSE, relative = TRUE) { processFiles(.self, path, target, action="symlink", overwrite=overwrite, relative=relative) }
        ), fileSet=.self))
    },
    
    findFormat = function (path, intent = c("read","write"), all = FALSE)
    {
        stem <- ensureFileSuffix(expandFileName(path), NULL, strip=unlist(formats))
        if (length(stem) > 1L)
            return (setNames(lapply(stem, .self$findFormat), path))
        
        intent <- match.arg(intent)
        auxPaths <- setNames(ensureFileSuffix(stem, auxiliaries), auxiliaries)
        if (intent == "read")
            auxPaths <- auxPaths[file.exists(auxPaths)]
        
        result <- NULL
        for (formatName in names(formats))
        {
            suffixes <- formats[[formatName]]
            paths <- ensureFileSuffix(stem, suffixes)
            if (intent == "write" || all(file.exists(paths)))
            {
                validator <- validators[[formatName]]
                if (intent == "read" && !is.null(validator) && !isTRUE(try(validator(setNames(paths, suffixes)), silent=TRUE)))
                    next
                
                if (is.null(result))
                {
                    result <- list(format=formatName, stem=stem, requiredFiles=setNames(paths,suffixes), auxiliaryFiles=auxPaths)
                    if (!all) break
                }
                else
                {
                    result$otherFormats <- c(result$otherFormats, formatName)
                    result$otherFiles <- c(result$otherFiles, paths)
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
        return (getRefClass(.self)$new(formats=subformats, validators=subvalidators), ...)
    }
))

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
    
    findFormat = function (path, all = FALSE)
    {
        stem <- ensureFileSuffix(expandFileName(path), NULL, strip=unlist(formats))
        if (length(stem) > 1L)
            return (setNames(lapply(stem, .self$findFormat), path))
        
        # If the stem matches literally then set header and image fields and return
        result <- callSuper(stem, all=all)
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
        
        mapFile <- file.path(dirname(stem), "map.yaml")
        if (file.exists(mapFile))
        {
            map <- yaml::read_yaml(mapFile)
            value <- map[[basename(stem)]]
            return (where(!is.null(value), findFormat(file.path(dirname(stem), value))))
        }
        else
        {
            value <- defaultMap[[basename(stem)]]
            return (where(!is.null(value), findFormat(file.path(dirname(stem), value))))
        }
    },
    
    mapTo = function (stem, target, overwrite = TRUE, relative = TRUE)
    {
        if (!arePresent(target))
            moveTo(stem, target, overwrite=overwrite)
        
        stem <- ensureFileSuffix(expandFileName(stem), NULL, strip=unlist(formats))
        for (s in stem)
        {
            mapFile <- file.path(dirname(s), "map.yaml")
            map <- where(file.exists(mapFile), yaml::read_yaml(mapFile), list())
            map[[basename(s)]] <- where(relative, relativePath(target,s), target)
            yaml::write_yaml(map, mapFile)
        }
    }
))

.ImageFiles <- ImageFileSet$new()

#' @export
imageFiles <- function (path = NULL)
{
    if (is.null(path))
        return (.ImageFiles)
    else
        return (.ImageFiles$atPath(path))
}
