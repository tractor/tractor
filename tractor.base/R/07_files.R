processFiles <- function (fileSet, stems, target, action = c("copy","move","symlink"), relative = TRUE)
{
    action <- match.arg(action)
    results <- list()
    
    dirTarget <- length(target) == 1L && dir.exists(target)
    assert(dirTarget || length(stems) == length(target), "If the target isn't an existing directory it should match the source in length")
    
    for (i in seq_along(stems))
    {
        info <- fileSet$findFormat(stems[i])
        if (is.null(info))
        {
            report(OL$Warning, "No valid format found for file stem #{stem}")
            next
        }
        
        sourceFiles <- c(info$requiredFiles, info$auxiliaryFiles)
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
            success <- file.copy(sourceFiles, targetFiles, overwrite=TRUE)
        else if (action == "move")
        {
            success <- file.copy(sourceFiles, targetFiles, overwrite=TRUE)
            if (all(success))
                unlink(sourceFiles)
        }
        else if (action == "symlink")
        {
            existingTargets <- !is.na(Sys.readlink(targetFiles))
            if (any(existingTargets))
                unlink(targetFiles[existingTargets])
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
#' alternative file formats or composite file types. \code{ImageFiles} is a
#' concrete instance of the class
#'
#' @field formats A named list mapping format names to required file suffixes.
#' @field validators An optional named list of validation functions for some
#'   or all of the supported formats.
#' @field auxiliaries A character vector of optional auxiliary file suffixes.
#' 
#' @export
FileSet <- setRefClass("FileSet", contains="TractorObject", fields=list(formats="list", validators="list", auxiliaries="character"), methods=list(
    findFormat = function (path)
    {
        stem <- ensureFileSuffix(expandFileName(path), NULL, strip=unlist(formats))
        if (length(stem) > 1L)
            return (setNames(lapply(stem, .self$findFormat), stem))
        
        for (formatName in names(formats))
        {
            suffixes <- formats[[formatName]]
            paths <- ensureFileSuffix(stem, suffixes)
            if (all(file.exists(paths)))
            {
                validator <- validators[[formatName]]
                if (!is.null(validator) && !isTRUE(try(validator(setNames(paths, suffixes)), silent=TRUE)))
                    next
                
                auxPaths <- setNames(ensureFileSuffix(stem, auxiliaries), auxiliaries)
                auxPaths <- auxPaths[file.exists(auxPaths)]
                
                return (list(format=formatName, stem=stem, requiredFiles=setNames(paths,suffixes), auxiliaryFiles=auxPaths))
            }
        }
        return (NULL)
    },
    
    arePresent = function (path)
    {
        if (length(path) == 1L)
            return (!is.null(findFormat(path)))
        else
            return (!sapply(findFormat(path), is.null))
    },

    copy = function (stem, target) { processFiles(.self, stem, target, action="copy") },

    move = function (stem, target) { processFiles(.self, stem, target, action="move") },

    symlink = function (stem, target, relative = TRUE) { processFiles(.self, stem, target, action="symlink", relative=relative) },

    delete = function (stem)
    {
        for (s in stem)
        {
            info <- findFormat(s)
            if (is.null(info))
            {
                report(OL$Warning, "No valid format found for file stem #{s}")
                next
            }
            filesToDelete <- c(info$requiredFiles, info$auxiliaryFiles)
            unlink(filesToDelete[file.exists(filesToDelete)], recursive=TRUE, expand=FALSE)
        }
    }
))

niftiVersionCheck <- function (versions)
{
    return (function(x) niftiVersion(x) %in% versions)
}

#' @rdName FileSet-class
#' @export
ImageFiles <- FileSet$new(formats=list(
    nifti="nii",
    nifti_gz="nii.gz",
    nifti_pair=c("hdr","img"),
    nifti_pair_gz=c("hdr.gz","img.gz"),
    mrtrix="mif",
    mrtrix_gz="mif.gz",
    mgh="mgh",
    mgh_gz="mgz",
    analyze=c("hdr","img"),
    analyze_gz=c("hdr.gz","img.gz")
), validators=list(
    nifti=niftiVersionCheck(1:2),
    nifti_gz=niftiVersionCheck(1:2),
    nifti_pair=niftiVersionCheck(1:2),
    nifti_pair_gz=niftiVersionCheck(1:2),
    analyze=niftiVersionCheck(0),
    analyze_gz=niftiVersionCheck(0)
), auxiliaries=c("lut","dirs","tags","json"))
