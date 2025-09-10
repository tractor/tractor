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
#' This reference class manages sets of related files based on a common file
#' stem and required and/or optional suffixes. It is designed for handling
#' alternative file formats or composite file types. The class represents the
#' group of file formats in the abstract; its methods handle specific files and
#' support operations such as copying, moving, symlinking, validating and
#' deleting those files, ensuring that all constituent files are handled
#' consistently.
#' 
#' @field formats A named list mapping format names to required file suffixes.
#' @field validators An optional named list of validation functions for some
#'   or all of the supported formats.
#' @field auxiliaries A character vector of optional auxiliary file suffixes.
#' 
#' @seealso \code{\linkS4class{ImageFileSet}} is a subclass specialised for
#'   image files.
#' @export
FileSet <- setRefClass("FileSet", contains="TractorObject", fields=list(formats="list", validators="list", auxiliaries="character"), methods=list(
    atPaths = function (paths)
    {
        "Return a handle object to manipulate files at specific paths"
        paths <- .self$resolvePaths(paths)
        .info <- .self$findFormat(paths, all=TRUE)
        
        return (structure(list(
            stems = function () { return (.self$fileStem(paths)) },
            
            info = function () { return (.info) },
            
            read = function (...)
            {
                images <- lapply(.info, function(i) {
                    where(!is.null(i), readImageFile(i$stem, ...))
                })
                return (where(length(images) == 1, images[[1]], images))
            },
            
            copy = function (target, overwrite = TRUE) { processFiles(.info, target, action="copy", overwrite=overwrite) },
            
            delete = function () { processFiles(.info, action="delete", all=TRUE) },
            
            move = function (target, overwrite = TRUE) { processFiles(.info, target, action="move", overwrite=overwrite) },
            
            present = function () { return (!sapply(.info, is.null)) },
            
            symlink = function (target, overwrite = FALSE, relative = TRUE) { processFiles(.info, target, action="symlink", overwrite=overwrite, relative=relative) }
        ), fileSet=.self, class="fileSetHandle"))
    },
    
    fileStem = function (paths)
    {
        "Obtain a file stem for each specified path, dropping format-specific suffixes"
        return (ensureFileSuffix(paths, NULL, strip=unlist(formats)))
    },
    
    findFormat = function (paths, intent = c("read","write"), preference = NULL, all = FALSE)
    {
        "Identify and potentially validate files to find the format used at specific paths"
        paths <- .self$resolvePaths(paths)
        intent <- match.arg(intent)
        results <- setNames(rep(list(NULL), length(paths)), paths)
        
        for (path in na.omit(paths))
        {
            stem <- .self$fileStem(path)
            auxPaths <- setNames(ensureFileSuffix(stem, auxiliaries), auxiliaries)
            if (intent == "read")
                auxPaths <- auxPaths[file.exists(auxPaths)]
            
            # Favour preferential formats, if any were specified
            formatNames <- names(formats)
            if (length(preference) > 0L)
            {
                preferred <- formatNames %in% as.character(preference)
                formatNames <- c(formatNames[preferred], formatNames[!preferred])
            }
            
            result <- NULL
            for (formatName in formatNames)
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
                        result <- structure(list(format=formatName, stem=stem, requiredFiles=setNames(realPaths,suffixes), auxiliaryFiles=auxPaths), class="concreteFileSet")
                        if (!all) break
                    }
                    else
                    {
                        result$otherFormats <- c(result$otherFormats, formatName)
                        result$otherFiles <- c(result$otherFiles, realPaths)
                    }
                }
            }
            
            # Avoid assigning NULL, which removes the element
            if (!is.null(result))
                results[[path]] <- result
        }
        
        return (results)
    },
    
    resolvePaths = function (paths)
    {
        "Perform any special path handling and resolve final paths"
        return (expandFileName(tractor.base::resolvePath(paths)))
    },
    
    subset = function (match, ...)
    {
        "Create a variant object that only encapsulates the specified subset of formats"
        regex <- where(is_ore(match), match, ore(match, options="i", syntax="fixed"))
        subformats <- formats[names(formats) %~% regex]
        subvalidators <- validators[names(validators) %~% regex]
        return (.self$getRefClass()$new(formats=subformats, validators=subvalidators, ...))
    }
))

#' @export
print.fileSetHandle <- function (x, ...)
{
    info <- x$info()
    n <- length(info)
    cat(es("Handle to #{n} file #{pluralise('set',n=n)}\n"))
    if (n == 1L)
    {
        suffixes <- c(names(info[[1]]$requiredFiles), names(info[[1]]$auxiliaryFiles))
        cat(es("- Path stem: #{x$stems()}\n"))
        if (is.null(info[[1]]))
            cat("- Format: (none)\n")
        else
            cat(es("- Format: \"#{info[[1]]$format}\" (#{pluralise('suffix',suffixes,plural='suffixes')} #{implode(suffixes,', ')})\n"))
    }
    cat(es("- Member functions: #{implode(names(x),', ')}\n"))
}

#' The FileMap class
#' 
#' A reference class to represent a file map and handle persisting it to file.
#' A file map is a directory-specific dictionary keyed by file names (without
#' suffixes), whose values point to the actual locations of the associated
#' files. This saves duplicating large files such as images, in a way similar
#' to symbolic linking, but without requiring special file system support, and
#' with only one link per file set rather than one per file. The map is
#' serialised in YAML format to a file called \code{map.yaml} in the relevant
#' directory. Reading from this file happens on object creation and when the
#' \code{read} method is called; writing is only by an explicit call to the
#' \code{write} method.
#' 
#' @field directory A character string representing the directory being mapped.
#' @field map A list representation of the map in memory.
#' 
#' @export
FileMap <- setRefClass("FileMap", contains="TractorObject", fields=list(directory="character", map="list"), methods=list(
    initialize = function (path = "", ...)
    {
        "Create a FileMap object for the specified paths"
        dir <- unique(expandFileName(ifelse(dir.exists(path), path, dirname(path))))
        assert(length(dir) > 0L, "No path was specified to initialise a map")
        assert(length(dir) == 1L, "All paths must be in the same directory")
        object <- initFields(directory=dir, map=list())
        object$read()
        return (object)
    },
    
    dropElements = function (keys)
    {
        "Remove elements with the specified keys from the in-memory map"
        .self$setElements(keys, NULL)
    },
    
    getDirectory = function () { return (directory) },
    
    getElements = function (keys)
    {
        "Return the values associated with the specified keys"
        return (unlist(map[keys]))
    },
    
    getFile = function ()
    {
        "Return the path to the map file, which may not yet exist"
        return (file.path(directory, "map.yaml"))
    },
    
    getMap = function (sorted = FALSE)
    {
        "Return the in-memory map, optionally sorted by key"
        where(sorted, map[sort(names(map))], map)
    },
    
    nElements = function () { return (length(map)) },
    
    setElements = function (keys, values)
    {
        "Replace the specified keys with new values, which should be strings"
        .self$map[keys] <- values
        invisible (.self)
    },
    
    read = function ()
    {
        "Read the map file into memory"
        file <- .self$getFile()
        if (directory != "" && file.exists(file))
            .self$map <- yaml::read_yaml(mapFile)
        invisible (.self)
    },
    
    write = function ()
    {
        "Write the in-memory map to file, or delete the file if it is empty"
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

#' The ImageFileSet class
#' 
#' \code{ImageFileSet} is a subclass of \code{FileSet} that is specialised for
#' images, by pre-populating the format and auxiliary file specification
#' (although this can be overridden) and by supporting file mapping using the
#' \code{FileMap} class. If a file set exists on disk and in a map then the
#' files take priority.
#' 
#' @field defaultMap A list representing a default file map which will be
#'   consulted in the absence of on-disk files or an existing file map to try
#'   to resolve paths.
#' 
#' @export
ImageFileSet <- setRefClass("ImageFileSet", contains="FileSet", fields=list(defaultMap="list"), methods=list(
    initialize = function (formats = .imageFormats, validators = .imageValidators, auxiliaries = c("dirs","lut","tags"), defaultMap = list(), ...)
    {
        object <- callSuper(formats=formats, validators=validators, auxiliaries=auxiliaries)
        object$defaultMap <- defaultMap
        return (object)
    },
    
    atPaths = function (paths)
    {
        "Return a handle object to manipulate files at specific paths"
        .super <- callSuper(paths)
        .info <- .super$info()
        .map <- FileMap$new(paths)
        
        result <- .super
        result$delete <- function ()
        {
            # Call .super, not .self, because for these purposes we don't want to resolve maps
            present <- .super$present()
            if (any(present))
                processFiles(.info[present], action="delete", all=TRUE)
            # Deleting a mapped image just unmaps it
            if (any(!present))
                .map$dropElements(basename(.self$fileStem(paths[!present])))$write()
        }
        result$map <- function (target, overwrite = TRUE, relative = TRUE)
        {
            singleTarget <- (length(target) == 1)
            targetsMissing <- sapply(.self$findFormat(target), is.null)
            if (any(targetsMissing))
                processFiles(.info[targetsMissing], where(singleTarget,target,target[targetsMissing]), action="move")
            if (any(.super$present()))
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
            for (i in seq_along(paths))
            {
                currentTarget <- where(singleTarget, target, target[i])
                currentPath <- where(relative, relativePath(currentTarget,paths[i]), currentTarget)
                .map$setElements(basename(.self$fileStem(paths[i])), currentPath)
            }
            .map$write()
            return (TRUE)
        }
        return (result)
    },
    
    findFormat = function (paths, intent = c("read","write"), all = FALSE)
    {
        # The superclass method calls .self$resolvePaths(), which handles map
        # redirection through the override below
        results <- callSuper(paths, intent=intent, preference=tolower(getOption("tractorFileType")), all=all)
        
        # Set header and image fields if files are found
        return (lapply(results, function(result) {
            if (is.null(result))
                return (NULL)
            if (length(result$requiredFiles) == 1L)
                result$headerFile <- result$imageFile <- result$requiredFiles
            else
            {
                result$headerFile <- result$requiredFiles[names(result$requiredFiles) %~|% "^hdr"]
                result$imageFile <- result$requiredFiles[names(result$requiredFiles) %~|% "^img"]
            }
            return (result)
        }))
    },
    
    resolvePaths = function (paths)
    {
        paths <- callSuper(paths)
        
        # Obtain a map and check if it's non-empty
        map <- FileMap$new(paths)
        if (map$nElements() == 0L && length(defaultMap) == 0L)
            return (paths)
        
        # Perform map redirection, if relevant
        stems <- sapply(.self$fileStem(paths), function(stem) {
            mapValue <- map$getElements(basename(stem)) %||% defaultMap[[basename(stem)]]
            if (!is.null(mapValue))
                stem <- file.path(dirname(stem), mapValue)
            return (stem)
        })
        return (stems)
    }
))

.ImageFiles <- ImageFileSet$new()

#' Image file sets
#' 
#' TractoR supports several medical imaging file formats, some of which are
#' made up of multiple files on disk or have compressed and uncompressed
#' variants. There is also support for auxiliary "sidecar" files, which have
#' the same path stem as the associated image files but different suffixes, and
#' contain additional metadata. Typically the image files are in a binary
#' format for space-efficiency, while any auxiliary files are in text-based
#' formats for easier human readability. This interface, and the unpinning
#' reference classes, handle and abstract away this complexity.
#' 
#' The \code{imageFiles} function returns either a preinitialised instance of
#' the \code{\linkS4class{ImageFileSet}} class, which handles file set logic
#' in the abstract, or (if given a vector of paths as an argument) calls that
#' instance's \code{atPaths} method, which facilitates handling specific files.
#' If non-default parameters to the class constructor are required then a
#' custom \code{ImageFileSet} object can be created directly instead.
#' 
#' \code{imageFileExists}, \code{removeImageFiles}, \code{symlinkImageFiles}
#' and \code{copyImageFiles} are simple wrapper functions that exist for
#' backwards compatibility. \code{copyImageFiles(from, to)}, for example, is
#' equivalent to \code{imageFiles(from)$copy(to)}.
#' 
#' @param paths Optionally, a character vector of image paths with or without
#'   suitable suffixes.
#' @param fileName,from Character vectors of image paths.
#' @param to Character vector of target file names, or a single existing
#'   directory to copy or symlink into.
#' @param overwrite Logical value: if \code{TRUE}, existing files will be
#'   overwritten; otherwise creating files with existing names will just fail.
#' @param relative Logical value: if \code{TRUE}, the path stored in the
#'   symlink will be relative (e.g. \code{"../some_dir/some_image.nii"}) rather
#'   than absolute (e.g. \code{"/path/to/some_dir/some_image.nii"}).
#' @param deleteOriginals Logical value: if \code{TRUE}, \code{copyImageFiles}
#'   performs a move rather than a copy.
#' @param \dots Currently unused.
#' @return If \code{paths} is \code{NULL}, \code{imageFiles} returns a
#'   singleton reference object of class \code{ImageFileSet} which can be used
#'   to identify and manipulate image files anywhere on the file system. If
#'   \code{paths} is specified, an S3 object of class \code{fileSetHandle}, a
#'   list of functions which can be used to manipulate the actual files at
#'   those paths. The functions are
#'   \describe{
#'     \item{stems()}{Return the file stems associated with the paths.}
#'     \item{info()}{Return information about existing files, as a list with
#'       one element per path. An element will be \code{NULL} if no
#'       corresponding files currently exist.}
#'     \item{read(...)}{Read the images into memory and return them as
#'       \code{\linkS4class{MriImage}} objects.}
#'     \item{copy(target, overwrite=TRUE)}{Copy the files to target paths (new
#'       file names or a directory).}
#'     \item{delete()}{Delete the files or any map reference to them.}
#'     \item{map(target, overwrite=TRUE, relative=TRUE)}{Map the files to a new
#'       location (see \code{\linkS4class{FileMap}} for details).}
#'     \item{move(target, overwrite=TRUE)}{Move the files to target paths (new
#'       file names or a directory).}
#'     \item{present()}{Return Boolean values indicating whether or not
#'       files exist at each path.}
#'     \item{symlink(target, overwrite=TRUE, relative=TRUE)}{Symlink the files
#'       to target paths (if supported by the OS and file system).}
#'   }
#'   \code{imageFileExists} returns a logical vector indicating whether or not
#'   valid image files exist at each specified path. Other functions are called
#'   for their side-effects.
#' @author Jon Clayden
#' @seealso Using \code{\linkS4class{ImageFileSet}} provides a lower-level and
#'   more flexible interface; \code{\link{readImageFile}} can be used if you
#'   just want to read an image into memory.
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @examples
#' path <- system.file("extdata", "analyze", "maskedb0.img.gz",
#'   package="tractor.base")
#' im <- imageFiles(path)
#' print(im)
#' im$present()
#' 
#' @export
imageFiles <- function (paths = NULL)
{
    if (is.null(paths))
        return (.ImageFiles)
    else
        return (.ImageFiles$atPaths(paths))
}

#' @rdname imageFiles
#' @export
imageFileExists <- function (fileName)
{
    return (imageFiles(fileName)$present())
}

#' @rdname imageFiles
#' @export
removeImageFiles <- function (fileName, ...)
{
    imageFiles(fileName)$delete()
}

#' @rdname imageFiles
#' @export
symlinkImageFiles <- function (from, to, overwrite = FALSE, relative = TRUE, ...)
{
    imageFiles(from)$symlink(to, overwrite=overwrite, relative=relative)
}

#' @rdname imageFiles
#' @export
copyImageFiles <- function (from, to, overwrite = FALSE, deleteOriginals = FALSE, ...)
{
    if (deleteOriginals)
        imageFiles(from)$move(to, overwrite=overwrite)
    else
        imageFiles(from)$copy(to, overwrite=overwrite)
}
