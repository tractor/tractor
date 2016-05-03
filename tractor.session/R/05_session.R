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
            updateSessionHierarchy(object)
            return (object)
        }
    },
    
    getDiffusionScheme = function (unrotated = FALSE)
    {
        diffusionDir <- .self$getDirectory("diffusion")
        
        if (unrotated && file.exists(file.path(diffusionDir,"directions-orig.txt")))
            fileName <- file.path(diffusionDir, "directions-orig.txt")
        else
            fileName <- file.path(diffusionDir, "directions.txt")
    
        if (file.exists(fileName))
        {
            gradientSet <- unname(as.matrix(read.table(fileName)))
            scheme <- SimpleDiffusionScheme$new(gradientSet[,4], gradientSet[,1:3])
            return (scheme)
        }
        else
            return (NULL)
    },
    
    getDirectory = function (type = NULL, createIfMissing = FALSE)
    {
        if (is.null(type))
            return (directory)
        else
        {
            type <- tolower(type)
            root <- file.path(directory, "tractor")
            
            if (type == "root")
                requiredDir <- root
            else if (!(type %in% names(caches.$subdirectories)))
                report(OL$Error, "Directory type \"#{type}\" is not valid")
            else
                requiredDir <- expandFileName(caches.$subdirectories[[type]], base=root)

            if (createIfMissing && !file.exists(requiredDir))
                dir.create(requiredDir, recursive=TRUE)
            return (requiredDir)
        }
    },
    
    getImageByType = function (type, place = NULL, index = 1, ...)
    {
        fileName <- .self$getImageFileNameByType(type, place, index)
        if (tolower(type) == "radialdiff" && !imageFileExists(fileName))
            createRadialDiffusivityMapForSession(.self)
        return (readImageFile(fileName, ...))
    },
    
    getImageFileNameByType = function (type, place = NULL, index = 1)
    {
        if (!is.null(place) && tolower(place) == "mni")
            return (getFileNameForStandardImage(type))
        else
        {
            type <- tolower(type)
            
            if (is.null(place))
            {
                locs <- which(sapply(caches.$maps, function(x) type %in% names(x)))
                if (length(locs) < 1)
                    report(OL$Error, "The specified file type (\"#{type}\") does not have a standard location")
                else if (length(locs) > 1)
                {
                    locs <- names(caches.$maps)[locs]
                    if (length(which(locs %in% .PrimarySessionDirectories)) == 1)
                        place <- locs[locs %in% .PrimarySessionDirectories]
                    else
                        report(OL$Error, "The specified file type (\"#{type}\") is ambiguous: it can exist in places #{implode(paste('\"',locs,'\"',sep=''),', ',finalSep=' and ')}")
                }
                else
                    place <- names(caches.$maps)[locs[1]]
            }
            
            map <- caches.$maps[[place]]
            names(map) <- tolower(names(map))
            directory <- .self$getDirectory(place)
            
            fileName <- map[[type]]
            if (is.null(fileName))
                report(OL$Error, "Image type \"#{type}\" is not valid")
            
            if (fileName %~% "\\%")
                fileName <- sapply(index, function(i) sub("%",as.character(i),fileName,fixed=TRUE))
            
            return (file.path(directory, fileName))
        }
    },
    
    getParcellation = function (place = "structural", ...)
    {
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
    
    getRegistrationTarget = function (space, ...)  { return (.self$getImageByType(.RegistrationTargets[[space]], space, ...)) },
    
    getRegistrationTargetFileName = function (space) { return (.self$getImageFileNameByType(.RegistrationTargets[[space]], space)) },
    
    getTracker = function (mask = NULL)
    {
        if (!("diffusionModel" %in% names(caches.$objects)))
        {
            if (.self$imageExists("avf", "bedpost"))
                .self$caches.$objects$diffusionModel <- tractor.track::bedpostDiffusionModel(.self$getDirectory("bedpost"))
            else
                report(OL$Error, "No diffusion model is available for the session with root directory #{.self$getDirectory()}")
        }
        
        if (is.null(mask))
            mask <- getImageFileNameByType("mask", "diffusion")
        
        return (tractor.track::Tracker$new(caches.$objects$diffusionModel, mask))
    },
    
    getTransformation = function (sourceSpace, targetSpace)
    {
        strategy <- caches.$transformStrategies[[es("#{sourceSpace}2#{targetSpace}")]]
        if ("reverse" %in% strategy)
            return (.self$getTransformation(targetSpace,sourceSpace)$invert())
        else
        {
            sourceImageFile <- .self$getRegistrationTargetFileName(sourceSpace)
            targetImageFile <- .self$getRegistrationTargetFileName(targetSpace)
            
            options <- list(sourceImageFile, targetImageFile, targetMask=NULL, estimateOnly=TRUE)
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
            
            fileHit <- FALSE
            transformDir <- file.path(.self$getDirectory("transforms",createIfMissing=TRUE), ensureFileSuffix(es("#{sourceSpace}2#{targetSpace}"),"xfmb"))
            transformFile <- file.path(.self$getDirectory("transforms",createIfMissing=TRUE), ensureFileSuffix(es("#{sourceSpace}2#{targetSpace}"),"Rdata"))
            if (file.exists(transformFile) && !file.exists(transformDir))
            {
                tractor.reg::attachTransformation(transformFile)$move(transformDir)
                unlink(transformFile)
            }
            if (file.exists(transformDir))
            {
                transform <- tractor.reg::attachTransformation(transformDir)
                if (all(options$types %in% transform$getTypes()))
                    return (transform)
            }
            
            report(OL$Info, "Transformation strategy from #{ifelse(sourceSpace=='mni','MNI',sourceSpace)} to #{ifelse(targetSpace=='mni','MNI',targetSpace)} space is #{implode(strategy,', ',' and ')} - registering images")
            result <- do.call(tractor.reg::registerImages, options)
            result$transform$move(transformDir)
            return (result$transform)
        }
    },
    
    imageExists = function (type, place = NULL, index = 1) { return (imageFileExists(.self$getImageFileNameByType(type, place, index))) },
    
    unlinkDirectory = function (type, ask = TRUE)
    {
        dirToRemove <- expandFileName(.self$getDirectory(type))
        rootDir <- expandFileName(.self$getDirectory("root"))
        
        if (!file.exists(dirToRemove))
            return (NULL)
        
        if (regexpr(rootDir, dirToRemove, fixed=TRUE) != 1)
            report(OL$Error, "Existing externally-mapped directory #{dirToRemove} will not be overwritten")
        else
        {
            ans <- (if (ask) ask("Directory #{dirToRemove} already exists. Delete it? [yn]") else "y")
            if (tolower(ans) == "y")
                unlink(dirToRemove, recursive=TRUE)
        }
    },
    
    updateCaches = function ()
    {
        defaultsPath <- file.path(Sys.getenv("TRACTOR_HOME"), "etc", "session")
        
        subdirectories <- readYaml(file.path(defaultsPath, "map.yaml"))
        mapFileName <- file.path(.self$getDirectory("root"), "map.yaml")
        if (file.exists(mapFileName))
            subdirectories <- deduplicate(readYaml(mapFileName), subdirectories)
        .self$caches.$subdirectories <- subdirectories
        
        maps <- list()
        for (place in .AllSessionDirectories)
        {
            defaultFileName <- file.path(defaultsPath, subdirectories[[place]], "map.yaml")
            if (!file.exists(defaultFileName))
                next
            maps[[place]] <- readYaml(defaultFileName)
            mapFileName <- file.path(.self$getDirectory(place), "map.yaml")
            if (file.exists(mapFileName))
                maps[[place]] <- deduplicate(readYaml(mapFileName), maps[[place]])
        }
        .self$caches.$maps <- maps
        
        transformStrategies <- readYaml(file.path(defaultsPath, "transforms", "strategy.yaml"))
        strategyFileName <- file.path(.self$getDirectory("transforms"), "strategy.yaml")
        if (file.exists(strategyFileName))
            transformStrategies <- deduplicate(readYaml(strategyFileName), transformStrategies)
        .self$caches.$transformStrategies <- transformStrategies
    },
    
    updateDiffusionScheme = function (scheme = NULL, unrotated = FALSE)
    {
        if (!is.null(scheme) && !is(scheme, "SimpleDiffusionScheme"))
            report(OL$Error, "Specified scheme is not a SimpleDiffusionScheme object")
        
        # No point in reading and writing from the same location at the same time
        if (is.null(scheme))
            scheme <- .self$getDiffusionScheme()
        else
        {
            diffusionDir <- .self$getDirectory("diffusion")
            gradientSet <- cbind(scheme$getGradientDirections(), scheme$getBValues())
            
            lines <- ore.subst("\\.0+\\s*$", "", apply(format(gradientSet,scientific=FALSE),1,implode,sep="  "))
            if (unrotated)
                writeLines(lines, file.path(diffusionDir,"directions-orig.txt"))
            else
                writeLines(lines, file.path(diffusionDir,"directions.txt"))
        }
        
        fslDir <- .self$getDirectory("fdt")
        if (!unrotated && file.exists(fslDir))
        {
            write.table(matrix(scheme$getBValues(),nrow=1), file.path(fslDir,"bvals"), row.names=FALSE, col.names=FALSE)
            write.table(t(scheme$getGradientDirections()), file.path(fslDir,"bvecs"), row.names=FALSE, col.names=FALSE)
        }
    }
))

attachMriSession <- function (directory)
{
    session <- MriSession$new(directory)
    invisible (session)
}

newSessionFromDirectory <- function (directory)
{
    return (attachMriSession(directory))
}

getImageCountForSession <- function (session, type, place = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    i <- 1
    while (session$imageExists(type, place, i))
        i <- i + 1
    
    return (i-1)
}

updateSessionHierarchy <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    oldFdtDirectory <- file.path(session$getDirectory("root"), "fdt")
    diffusionDirectory <- session$getDirectory("diffusion")
    
    if (file.exists(oldFdtDirectory) && !file.exists(diffusionDirectory))
    {
        # Assume this is a TractoR 1.x session directory: rename the FDT
        # directory, insert the appropriate map, and move FDT-only files back
        file.rename(oldFdtDirectory, diffusionDirectory)
        writeYaml(.FdtDiffusionMap, file.path(diffusionDirectory, "map.yaml"), capitaliseLabels=FALSE)
        
        filesToMoveBack <- c("bvals", "bvecs", "data.ecclog")
        filesToMoveBack <- filesToMoveBack[file.exists(file.path(diffusionDirectory,filesToMoveBack))]
        if (length(filesToMoveBack) > 0)
        {
            newFdtDirectory <- session$getDirectory("fdt", createIfMissing=TRUE)
            file.rename(file.path(diffusionDirectory,filesToMoveBack), file.path(newFdtDirectory,filesToMoveBack))
        }
        
        newFdtDirectory <- session$getDirectory("fdt")
        if (file.exists(file.path(newFdtDirectory,"bvals")) && file.exists(file.path(newFdtDirectory,"bvecs")))
        {
            bValues <- unlist(read.table(file.path(newFdtDirectory, "bvals")))
            bVectors <- as.matrix(read.table(file.path(newFdtDirectory, "bvecs")))
            scheme <- SimpleDiffusionScheme$new(bValues, t(bVectors))
            session$updateDiffusionScheme(scheme)
        }
        
        # Update caches before creating FDT files, so that everything can be found
        session$updateCaches()
        
        createFdtFilesForSession(session)
        
        objectDirectory <- file.path(session$getDirectory("root"), "objects")
        if (file.exists(objectDirectory))
            unlink(objectDirectory, recursive=TRUE)
    }
}

createRadialDiffusivityMapForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    secondEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=2)
    thirdEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=3)
    radialDiffusivity <- secondEigenvalue$map(function(x,y) (x+y)/2, thirdEigenvalue)
    
    writeImageFile(radialDiffusivity, session$getImageFileNameByType("radialdiff"))
}
