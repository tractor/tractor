.resolveRegistrationTargets <- function (session, sourceSpace, targetSpace)
{
    `%with%` <- function (X,Y) { if (is.null(X) || is.null(Y)) NULL else list(X,Y) }
    
    registrand <- function (mode, space)
    {
        type <- .RegistrationTargets[[space]][[mode]]
        if (!is.null(type))
        {
            files <- session$imageFiles(type, space)
            if (files$present())
            {
                # File types RNifti can't handle need to be read by TractoR
                if (files$info()[[1]]$format %~% "^(analyze|nifti)")
                    return (files$stems())
                else
                    return (files$read(reorder=FALSE))
            }
        }
        return (NULL)
    }
    
    return ((registrand("unmasked",sourceSpace) %with% registrand("unmasked",targetSpace)) %||%
            (registrand("masked",sourceSpace) %with% registrand("masked",targetSpace)) %||% {
                flag(OL$Warning, "Coregistering images with and without brain extraction may not produce good results")
                sourceMode <- names(.RegistrationTargets[[sourceSpace]])[1]
                targetMode <- names(.RegistrationTargets[[targetSpace]])[1]
                registrand(sourceMode,sourceSpace) %with% registrand(targetMode,targetSpace)
            })
}

#' The MriSession class
#'
#' This class represents a single MRI scan session: a managed hierarchy of
#' directories and files, rooted at a particular location on disk, which may
#' include structural, diffusion-weighted and/or functional images along with
#' associated metadata and derived data (masks, tensors, registrations,
#' tractography results, etc.). The standard layout of a session, and the
#' mapping from standardised image "type" names (such as `"rawdata"`, `"mask"`
#' or `"fa"`) to file names within each subdirectory, is described by "map"
#' YAML files under `share/tractor/session` in the TractoR installation, and
#' may be extended or overridden on a per-session basis by placing a
#' `map.yaml` file in the relevant directory. This class provides methods to
#' resolve directory and image file paths for a session without the caller
#' needing to know the details of its layout, to read and write images and
#' other data by type, and to obtain (creating if necessary) registrations
#' between the various spaces - structural, diffusion, functional and
#' standard/MNI - associated with the session.
#'
#' @field directory The absolute path to the root of the session, i.e. the
#'   parent of the `tractor` subdirectory in which TractoR-managed files and
#'   metadata are stored.
#' @field caches. A list of information cached for efficiency, including the
#'   session's subdirectory and per-directory file maps, its registration
#'   strategies, and objects such as a diffusion model created for
#'   tractography. Rebuilt as needed by the `updateCaches()` method. This
#'   field is not serialised with the object.
#'
#' @note Session objects are usually created using the [attachMriSession()]
#'   function rather than calling `MriSession$new()` directly.
#' @export
MriSession <- setRefClass("MriSession", contains="SerialisableObject", fields=list(directory="character",caches.="list"), methods=list(
    initialize = function (directory = NULL, ...)
    {
        if (is.null(directory))
            return (initFields(directory=""))
        else if (length(directory) != 1)
            report(OL$Error, "Session directory name should have length 1")
        else if (!file.exists(directory))
            report(OL$Error, "Session directory does not exist")
        else
        {
            object <- initFields(directory=expandFileName(directory), caches.=list())
            object$updateCaches()
            object$caches.$objects <- list()
            object$getDirectory("root", createIfMissing=TRUE)
            return (object)
        }
    },
    
    getDiffusionScheme = function (unrotated = FALSE)
    {
        "Get the diffusion scheme (b-values and gradient directions) associated with the session"
        # The argument means unrotated only; otherwise rotated is preferred but not required
        diffusionDir <- .self$getDirectory("diffusion")
        scheme <- readDiffusionScheme(.self$getImageFileNameByType(ifelse(unrotated,"rawdata","data"), "diffusion"))
        if (is.null(scheme) && !unrotated)
            scheme <- readDiffusionScheme(.self$getImageFileNameByType("rawdata","diffusion"))
        if (is.null(scheme))
        {
            if (unrotated && file.exists(file.path(diffusionDir,"directions-orig.txt")))
                fileName <- file.path(diffusionDir, "directions-orig.txt")
            else
                fileName <- file.path(diffusionDir, "directions.txt")
            
            if (file.exists(fileName))
                scheme <- readDiffusionScheme(fileName)
        }
        return (scheme)
    },
    
    getDirectory = function (type = NULL, createIfMissing = FALSE)
    {
        "Obtain the path to the session's root, or one of its standard subdirectories, optionally creating it"
        if (is.null(type))
            return (directory)
        else
        {
            type <- tolower(type)
            root <- file.path(directory, "tractor")
            if (is.null(caches.$subdirectories))
                subdirs <- list()
            else
                subdirs <- structure(caches.$subdirectories, names=tolower(names(caches.$subdirectories)))
            
            if (type == "root")
                requiredDir <- root
            else if (!(type %in% names(subdirs)))
                report(OL$Error, "Directory type \"#{type}\" is not valid")
            else
                requiredDir <- expandFileName(subdirs[[type]], base=root)
            
            if (createIfMissing && !file.exists(requiredDir))
                dir.create(requiredDir, recursive=TRUE)
            return (requiredDir)
        }
    },
    
    getImageByType = function (type, place = NULL, index = 1, ...)
    {
        "Read an image of the specified standard type from the session"
        fileName <- .self$getImageFileNameByType(type, place, index)
        if (tolower(type) == "radialdiff" && !imageFileExists(fileName))
            createRadialDiffusivityMapForSession(.self)
        return (readImageFile(fileName, ...))
    },
    
    getImageFileNameByType = function (type, place = NULL, index = 1, fallback = FALSE)
    {
        "Resolve the file name for an image of a particular standard type, optionally in a particular subdirectory"
        if (!is.null(place) && tolower(place) == "mni")
            return (getFileNameForStandardImage(type))
        else
        {
            type <- tolower(type)
            maps <- structure(caches.$maps, names=tolower(names(caches.$maps)))
            
            if (is.null(place))
            {
                locs <- which(sapply(maps, function(x) type %in% tolower(names(x))))
                if (length(locs) < 1)
                    report(OL$Error, "The specified file type (\"#{type}\") does not have a standard location")
                else if (length(locs) > 1)
                {
                    locs <- names(maps)[locs]
                    if (length(which(locs %in% .PrimarySessionDirectories)) == 1)
                        place <- locs[locs %in% .PrimarySessionDirectories]
                    else
                        report(OL$Error, "The specified file type (\"#{type}\") is ambiguous: it can exist in places #{implode(paste('\"',locs,'\"',sep=''),', ',finalSep=' and ')}")
                }
                else
                    place <- names(maps)[locs[1]]
            }
            else
                place <- names(maps)[pmatch(tolower(place), names(maps))]
            
            map <- structure(maps[[place]], names=tolower(names(maps[[place]])))
            directory <- .self$getDirectory(place)
            
            fileName <- map[[type]]
            if (is.null(fileName))
            {
                if (fallback)
                    return (file.path(directory, type))
                else
                    report(OL$Error, "Image type \"#{type}\" is not valid")
            }
            
            if (fileName %~% "\\%")
                fileName <- sapply(index, function(i) sub("%",as.character(i),fileName,fixed=TRUE))
            if (fileName %~% "@")
                fileName <- ore.subst("@", basename(.self$getDirectory()), fileName)
            
            return (structure(file.path(directory,fileName), place=place))
        }
    },
    
    getMap = function (place = "root")
    {
        "Obtain the file name map for the session's root directory, or a particular subdirectory"
        .self$updateCaches()
        if (place == "root")
            return (.self$caches.$subdirectories)
        else
            return (.self$caches.$maps[[place]])
    },
    
    getParcellation = function (place = "structural", ...)
    {
        "Obtain a parcellation for the session, generating it by transformation from structural space if necessary"
        fileName <- .self$getImageFileNameByType("parcellation", place)
        if (!imageFileExists(fileName))
        {
            if (place == "structural")
                report(OL$Error, "T1w image parcellation has not yet been performed")
            else
            {
                parcellation <- transformParcellationToSpace(.self$getParcellation("structural"), .self, place, ...)
                tractor.reg::writeParcellation(parcellation, fileName)
            }
        }
        else
            parcellation <- tractor.reg::readParcellation(fileName)
        
        return (parcellation)
    },
    
    getRegistrationTarget = function (space, ...)  { return (.self$getImageByType(.RegistrationTargets[[space]][[1]], space, ...)) },
    
    getRegistrationTargetFileName = function (space) { return (.self$getImageFileNameByType(.RegistrationTargets[[space]][[1]], space)) },
    
    getTracker = function (mask = NULL, preferredModel = c("bedpost","dti"), ...)
    {
        "Create a streamline tracker using the session's diffusion model, caching the model for reuse"
        preferredModel <- match.arg(preferredModel)
        availableModels <- c(.self$imageExists("avf", "bedpost"),
                             .self$imageExists("eigenvector", "diffusion", 1))
        names(availableModels) <- c("bedpost", "dti")
        
        if (!any(availableModels))
            report(OL$Error, "No diffusion model is available for the session with root directory #{.self$getDirectory()}")
        else if (!availableModels[preferredModel])
        {
            preferredModel <- names(which(availableModels))
            flag(OL$Warning, "Preferred diffusion model is not available - reverting to #{toupper(preferredModel)}")
        }
        
        if (!("diffusionModel" %in% names(caches.$objects)) || .self$caches.$objects$diffusionModel$getType() != preferredModel)
        {
            if (preferredModel == "bedpost")
                .self$caches.$objects$diffusionModel <- tractor.track::bedpostDiffusionModel(.self$getDirectory("bedpost"))
            else
                .self$caches.$objects$diffusionModel <- tractor.track::dtiDiffusionModel(.self$getImageFileNameByType("eigenvector", "diffusion", 1))
        }
        
        if (is.null(mask))
            mask <- getImageFileNameByType("mask", "diffusion")
        
        return (createTracker(caches.$objects$diffusionModel, mask, ...))
    },
    
    getTransformation = function (sourceSpace, targetSpace)
    {
        "Obtain a registration between two spaces associated with the session, running one if a suitable transformation does not already exist on disk"
        strategy <- caches.$transformStrategies[[es("#{sourceSpace}2#{targetSpace}")]]
        if ("reverse" %in% strategy)
            return (.self$getTransformation(targetSpace,sourceSpace)$reverse())
        else
        {
            options <- list(targetMask=NULL, estimateOnly=TRUE)
            options$types <- "affine"
            if ("fsl" %in% strategy)
                options$method <- "fsl"
            else if ("niftyreg" %in% strategy)
                options$method <- "niftyreg"
            if ("nonlinear" %in% strategy)
            {
                options$types <- c(options$types, "nonlinear")
                options$method <- "niftyreg"
            }
            if (all(c("nonlinear","symmetric") %in% strategy))
                options$types <- c(options$types, "reverse-nonlinear")
            
            registration <- NULL
            regPath <- file.path(.self$getDirectory("transforms",createIfMissing=TRUE), es("#{sourceSpace}2#{targetSpace}"))
            if (any(file.exists(ensureFileSuffix(regPath, c("Rdata","xfmb")))))
            {
                # If this doesn't contain the required transforms it will be updated below
                registration <- tractor.reg::readRegistration(regPath)
                if (all(options$types %in% names(registration$getTypes())))
                    return (registration)
            }
            
            # No suitable registration exists, so we need to run one
            images <- .resolveRegistrationTargets(.self, sourceSpace, targetSpace)
            options <- c(images, list(registration=registration), options)
            
            report(OL$Info, "Transformation strategy from #{ifelse(sourceSpace=='mni','MNI',sourceSpace)} to #{ifelse(targetSpace=='mni','MNI',targetSpace)} space is #{implode(strategy,', ',' and ')} - registering images")
            registration <- do.call(tractor.reg::registerImages, options)
            
            registration$serialise(regPath)
            if (dir.exists(ensureFileSuffix(regPath, "xfmb")))
                unlink(ensureFileSuffix(regPath,"xfmb"), recursive=TRUE)
            
            return (registration)
        }
    },
    
    imageExists = function (type, place = NULL, index = 1) { return (imageFileExists(.self$getImageFileNameByType(type, place, index))) },
    
    imageFiles = function (type, place = NULL, index = 1, fallback = FALSE)
    {
        "Obtain a file-set handle for an image of a particular standard type"
        path <- .self$getImageFileNameByType(type, place, index, fallback)
        if (!is.null(place) && tolower(place) == "mni")
            return (tractor.base::imageFiles(path))
        else
        {
            place <- place %||% attr(path, "place")
            fileSet <- tractor.base:::ImageFileSet(defaultMap=.self$getMap(place))
            return (fileSet$atPaths(path))
        }
    },
    
    unlinkDirectory = function (type, ask = TRUE)
    {
        "Delete one of the session's subdirectories, or unmap it if it lies outside the session"
        dirToRemove <- expandFileName(.self$getDirectory(type))
        rootDir <- expandFileName(.self$getDirectory("root"))
        
        if (!file.exists(dirToRemove))
            return (NULL)
        
        if (regexpr(rootDir, dirToRemove, fixed=TRUE) != 1)
        {
            report(OL$Info, "Unmapping external directory #{dirToRemove}")
            mapFileName <- file.path(.self$getDirectory("root"), "map.yaml")
            subdirectoryMap <- read_yaml(mapFileName)
            subdirectoryMap <- subset(subdirectoryMap, tolower(names(subdirectoryMap)) != tolower(type))
            write_yaml(subdirectoryMap, mapFileName)
        }
        else if (!ask || reportr::ask("Directory #{dirToRemove} already exists. Delete it? [yn]", valid=c("y","n")) == "y")
            unlink(dirToRemove, recursive=TRUE)
    },
    
    updateCaches = function ()
    {
        "Rebuild the session's cached subdirectory and file maps and registration strategies from the relevant map.yaml files"
        bidsDescription <- file.path(directory, "..", "dataset_description.json")
        if (file.exists(bidsDescription) && ore.file(bidsDescription) %~% "BIDSVersion")
            defaultsPath <- file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "session", "bids")
        else
            defaultsPath <- file.path(Sys.getenv("TRACTOR_HOME"), "share", "tractor", "session", "default")
        
        subdirectories <- defaultSubdirectories <- yaml.load_file(file.path(defaultsPath, "map.yaml"))
        mapFileName <- file.path(.self$getDirectory("root"), "map.yaml")
        if (file.exists(mapFileName))
            subdirectories <- deduplicate(yaml.load_file(mapFileName), subdirectories)
        .self$caches.$subdirectories <- subdirectories
        
        maps <- list()
        for (place in .AllSessionDirectories)
        {
            defaultFileName <- file.path(defaultsPath, basename(defaultSubdirectories[[place]]), "map.yaml")
            if (!file.exists(defaultFileName))
                next
            maps[[place]] <- yaml.load_file(defaultFileName)
            mapFileName <- file.path(.self$getDirectory(place), "map.yaml")
            if (file.exists(mapFileName))
                maps[[place]] <- deduplicate(yaml.load_file(mapFileName), maps[[place]])
        }
        .self$caches.$maps <- maps
        
        transformStrategies <- yaml.load_file(file.path(defaultsPath, "transforms", "strategy.yaml"))
        strategyFileName <- file.path(.self$getDirectory("transforms"), "strategy.yaml")
        if (file.exists(strategyFileName))
            transformStrategies <- deduplicate(yaml.load_file(strategyFileName), transformStrategies)
        .self$caches.$transformStrategies <- transformStrategies
    },
    
    updateDiffusionScheme = function (scheme = NULL, unrotated = FALSE)
    {
        "Write a diffusion scheme to file for the session, also updating the FSL-style bvals/bvecs files if appropriate"
        if (!is.null(scheme) && !is(scheme, "DiffusionScheme"))
            report(OL$Error, "Specified scheme is not a DiffusionScheme object")
        
        # No point in reading and writing from the same location at the same time
        if (is.null(scheme))
            scheme <- .self$getDiffusionScheme()
        else
        {
            fileName <- ensureFileSuffix(.self$getImageFileNameByType(ifelse(unrotated,"rawdata","data"), "diffusion"), "dirs")
            scheme$writeToFile(fileName)
        }
        
        fslDir <- .self$getDirectory("fdt")
        if (!unrotated && file.exists(fslDir))
        {
            write.table(matrix(scheme$getBValues(),nrow=1), file.path(fslDir,"bvals"), row.names=FALSE, col.names=FALSE)
            write.table(t(scheme$getGradientDirections()), file.path(fslDir,"bvecs"), row.names=FALSE, col.names=FALSE)
        }
    }
))

setAs("character", "MriSession", function (from) {
    return (MriSession$new(from))
})

setAs("MriSession", "character", function (from) {
    return (from$getDirectory())
})

#' @export
as.character.MriSession <- function (x, ...)
{
    return (x$getDirectory())
}

#' Attach a session directory
#'
#' These functions create an [MriSession] object representing an existing
#' session directory on disk.
#'
#' @param directory A string giving the path to the session directory (i.e.
#'   the parent of the `tractor` subdirectory in which TractoR-managed files
#'   live).
#' @return An [MriSession] object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
attachMriSession <- function (directory)
{
    session <- MriSession$new(directory)
    invisible (session)
}

#' @rdname attachMriSession
#' @export
newSessionFromDirectory <- function (directory)
{
    return (attachMriSession(directory))
}

#' Count the images of a particular type associated with a session
#'
#' This function counts the number of images of a particular type associated
#' with a session, using the `index` mechanism supported by some image types
#' (such as per-fibre `"avf"` maps produced by FSL-BEDPOSTX). It works by
#' testing sequentially-indexed images for existence until one is not found.
#'
#' @param session An [MriSession] object.
#' @param type A string giving the (session-relative) type of image required.
#' @param place A string giving the subdirectory in which the image should be
#'   found, or `NULL` to look in the type's standard location.
#' @return The number of images of the specified type found, which may be
#'   zero.
#' @author Jon Clayden
#' @export
getImageCountForSession <- function (session, type, place = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    i <- 1
    while (session$imageExists(type, place, i))
        i <- i + 1
    
    return (i-1)
}

#' Create a radial diffusivity map for a session
#'
#' This function creates a radial diffusivity map for a session with an
#' existing diffusion tensor fit, calculated as the mean of the second and
#' third eigenvalues of the tensor at each voxel, and writes it to the
#' session's diffusion directory.
#'
#' @param session An [MriSession] object, which must already have second and
#'   third eigenvalue maps associated with it (as produced by
#'   [createDiffusionTensorImagesForSession()], for example).
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @export
createRadialDiffusivityMapForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    secondEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=2)
    thirdEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=3)
    radialDiffusivity <- secondEigenvalue$map(function(x,y) (x+y)/2, thirdEigenvalue)
    
    writeImageFile(radialDiffusivity, session$getImageFileNameByType("radialdiff"))
}
