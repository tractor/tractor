MriSession <- setRefClass("MriSession", contains="SerialisableObject", fields=list(directory="character",subdirectoryCache.="list",mapCache.="list",transformStrategyCache.="list",objectCache.="list"), methods=list(
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
            object <- initFields(directory=expandFileName(directory), subdirectoryCache.=list(), mapCache.=list(), objectCache.=list())
            object$updateCaches()
            object$getDirectory("root", createIfMissing=TRUE)
            updateSessionHierarchy(object)
            return (object)
        }
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
            else if (!(type %in% names(subdirectoryCache.)))
                report(OL$Error, "Directory type \"", type, "\" is not valid")
            else
                requiredDir <- expandFileName(subdirectoryCache.[[type]], base=root)

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
            return (getImageFileNameForSession(.self, type, place, index))
    },
    
    getMap = function (place) { return (mapCache.[[place]]) },
    
    getParcellation = function (place = "structural", ...)
    {
        fileName <- .self$getImageFileNameByType("parcellation", place)
        if (!imageFileExists(fileName))
        {
            if (place == "structural")
                report(OL$Error, "T1w image parcellation has not yet been performed")
            else
            {
                parcellation <- tractor.reg::transformParcellationToSpace(.self$getParcellation("structural"), .self, place, ...)
                tractor.reg::writeParcellation(parcellation, fileName)
            }
        }
        else
            parcellation <- tractor.reg::readParcellation(fileName)
        
        return (parcellation)
    },
    
    getRegistrationTarget = function (space, ...)  { return (.self$getImageByType(.RegistrationTargets[[space]], space, ...)) },
    
    getRegistrationTargetFileName = function (space) { return (.self$getImageFileNameByType(.RegistrationTargets[[space]], space)) },
    
    getTracker = function (mask = NULL, targets = NULL)
    {
        if (!("diffusionModel" %in% names(objectCache.)))
        {
            if (.self$imageExists("avf", "bedpost"))
                objectCache.$diffusionModel <- tractor.track::bedpostDiffusionModel(.self$getDirectory("bedpost"))
            else
                report(OL$Error, "No diffusion model is available for the session with root directory #{.self$getDirectory()}")
        }
        
        if (is.null(mask))
            mask <- getImageFileNameByType("mask", "diffusion")
        
        return (tractor.track::Tracker$new(objectCache.$diffusionModel, mask, targets))
    },
    
    getTransformation = function (sourceSpace, targetSpace)
    {
        strategy <- transformStrategyCache.[[s("#{sourceSpace}2#{targetSpace}")]]
        if ("reverse" %in% strategy)
            return (tractor.reg::invertTransformation(.self$getTransformation(targetSpace, sourceSpace)))
        else
        {
            sourceImageFile <- .self$getRegistrationTargetFileName(sourceSpace)
            targetImageFile <- .self$getRegistrationTargetFileName(targetSpace)
            transformFile <- file.path(.self$getDirectory("transforms",createIfMissing=TRUE), ensureFileSuffix(paste(sourceSpace,"2",targetSpace,sep=""),"Rdata"))
            
            # Injected option for backwards compatibility
            targetMask <- NULL
            if (sourceSpace == "diffusion" && targetSpace == "mni")
                targetMask <- newMriImageWithSimpleFunction(getStandardImage("white"), function(x) x/10 + 1)
            
            options <- list(sourceImageFile, targetImageFile, targetMask=targetMask, estimateOnly=TRUE, cache="ignore", file=transformFile)
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
            result <- do.call(tractor.reg::registerImages, options)
            
            reverseTransformFile <- file.path(.self$getDirectory("transforms"), ensureFileSuffix(paste(targetSpace,"2",sourceSpace,sep=""),"Rdata"))
            if (!file.exists(reverseTransformFile))
            {
                inverseTransform <- tractor.reg::invertTransformation(result$transform, quiet=TRUE)
                inverseTransform$serialise(reverseTransformFile)
            }
            
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
            if (ask)
                ans <- ask("Directory #{dirToRemove} already exists. Delete it? [yn]")
            else
                ans <- "y"
            
            if (tolower(ans) == "y")
                unlink(dirToRemove, recursive=TRUE)
        }
    },
    
    updateCaches = function ()
    {
        mapFileName <- file.path(.self$getDirectory("root"), "map.yaml")
        if (file.exists(mapFileName))
        {
            subdirectories <- c(readYaml(mapFileName), .DefaultSessionDirectories)
            subdirectories <- subdirectories[!duplicated(names(subdirectories))]
        }
        else
            subdirectories <- .DefaultSessionDirectories
            
        .self$subdirectoryCache. <- subdirectories
            
        maps <- list()
        for (place in names(.DefaultSessionMap))
        {
            mapFileName <- file.path(.self$getDirectory(place), "map.yaml")
            if (file.exists(mapFileName))
            {
                currentMap <- c(readYaml(mapFileName), .DefaultSessionMap[[place]])
                maps[[place]] <- currentMap[!duplicated(names(currentMap))]
            }
            else
                maps[[place]] <- .DefaultSessionMap[[place]]
        }
            
        .self$mapCache. <- maps
        
        transformStrategies <- readYaml(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "session", "transforms", "strategy.yaml"))
        strategyFileName <- file.path(.self$getDirectory("transforms"), "strategy.yaml")
        if (file.exists(strategyFileName))
        {
            transformStrategies <- c(readYaml(strategyFileName), transformStrategies)
            transformStrategies <- transformStrategies[!duplicated(names(transformStrategies))]
        }
        
        .self$transformStrategyCache. <- transformStrategies
    }
))

newSessionFromDirectory <- function (directory)
{
    session <- MriSession$new(directory)
    invisible (session)
}

getImageFileNameForSession <- function (session, type, place = NULL, index = 1)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    type <- tolower(type)
    
    if (is.null(place))
    {
        locs <- which(sapply(.DefaultSessionMap, function(x) type %in% names(x)))
        if (length(locs) < 1)
            report(OL$Error, "The specified file type (\"#{type}\") does not have a standard location")
        else if (length(locs) > 1)
        {
            locs <- names(.DefaultSessionMap)[locs]
            if (length(which(locs %in% .PrimarySessionDirectories)) == 1)
                place <- locs[locs %in% .PrimarySessionDirectories]
            else
                report(OL$Error, "The specified file type (\"#{type}\") is ambiguous: it can exist in places #{implode(paste('\"',locs,'\"',sep=''),', ',finalSep=' and ')}")
        }
        else
            place <- names(.DefaultSessionMap)[locs[1]]
    }
    
    map <- session$getMap(place)
    names(map) <- tolower(names(map))
    directory <- session$getDirectory(place)
    
    fileName <- map[[type]]
    if (is.null(fileName))
        report(OL$Error, "Image type \"", type, "\" is not valid")
    
    if (fileName %~% "\\%")
        fileName <- sapply(index, function(i) sub("%",as.character(i),fileName,fixed=TRUE))
    
    filePath <- file.path(directory, fileName)
    
    return (filePath)
}

getImageCountForSession <- function (session, type, place = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    i <- 1
    while (imageFileExists(getImageFileNameForSession(session, type, place, index=i)))
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
            scheme <- newSimpleDiffusionSchemeWithDirections(bVectors, bValues)
            writeSimpleDiffusionSchemeForSession(session, scheme)
        }
        
        # Update caches before creating FDT files, so that everything can be found
        session$updateCaches()
        
        createFdtFilesForSession(session)
        
        objectDirectory <- file.path(session$getDirectory("root"), "objects")
        if (file.exists(objectDirectory))
            unlink(objectDirectory, recursive=TRUE)
    }
}

standardiseSessionHierarchy <- function (session, includeDirectories = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    for (dirName in names(.DefaultSessionMap))
    {
        directory <- session$getDirectory(dirName)
        mapFileName <- file.path(directory, "map.yaml")
        if (file.exists(mapFileName))
        {
            map <- readYaml(mapFileName)
            names <- intersect(names(map), names(.DefaultSessionMap[[dirName]]))
            files <- file.path(directory, unlist(map[names]))
            
            nonImageFiles <- unlist(.DefaultSessionMap[[dirName]][names]) %~% "\\."
            wildcardFiles <- unlist(.DefaultSessionMap[[dirName]][names]) %~% "\\%"
            otherFiles <- !(nonImageFiles | wildcardFiles)
            
            if (any(nonImageFiles))
            {
                fileExistence <- file.exists(files[nonImageFiles])
                currentNames <- names[nonImageFiles][fileExistence]
                currentFiles <- files[nonImageFiles][fileExistence]
                file.rename(currentFiles, file.path(directory,unlist(.DefaultSessionMap[[dirName]][currentNames])))
            }
            if (any(wildcardFiles))
            {
                # Wildcard files are assumed to be images
                for (i in 1:9)
                {
                    currentFiles <- sub("\\%", i, files[wildcardFiles], perl=TRUE)
                    fileExistence <- imageFileExists(currentFiles)
                    currentNames <- names[wildcardFiles][fileExistence]
                    currentFiles <- currentFiles[fileExistence]
                    currentTargets <- sub("\\%", i, unlist(.DefaultSessionMap[[dirName]][currentNames]), perl=TRUE)
                    copyImageFiles(currentFiles, file.path(directory,currentTargets), deleteOriginals=TRUE)
                }
            }
            if (any(otherFiles))
            {
                fileExistence <- imageFileExists(files[otherFiles])
                currentNames <- names[otherFiles][fileExistence]
                currentFiles <- files[otherFiles][fileExistence]
                copyImageFiles(currentFiles, file.path(directory,unlist(.DefaultSessionMap[[dirName]][currentNames])), deleteOriginals=TRUE)
            }
            
            unlink(mapFileName)
        }
    }
    
    # We don't change the directory map by default, since it can point to another session, and so the move could break things
    mapFileName <- file.path(session$getDirectory("root"), "map.yaml")
    if (includeDirectories && file.exists(mapFileName))
    {
        map <- readYaml(mapFileName)
        names <- intersect(names(map), names(.DefaultSessionDirectories))
        for (name in names)
        {
            oldLoc <- session$getDirectory(name)
            newLoc <- expandFileName(.DefaultSessionDirectories[[name]], base=session$getDirectory("root"))
            file.rename(oldLoc, newLoc)
        }
        
        unlink(mapFileName)
    }
    
    session$updateCaches()
}

createCaminoFilesForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    session$unlinkDirectory("camino", ask=TRUE)
    caminoDir <- session$getDirectory("camino", createIfMissing=TRUE)
    
    sourceDir <- session$getDirectory("fdt")
    if (!file.exists(file.path(sourceDir,"bvals")) || !file.exists(file.path(sourceDir,"bvecs")) || !imageFileExists(session$getImageFileNameByType("data","diffusion")) || !imageFileExists(session$getImageFileNameByType("mask","diffusion")))
        report(OL$Error, "Some required files are missing - the session must be processed for FSL first")
    
    # Having created the Camino directory, reading and writing back the
    # scheme is sufficient to create the Camino file
    report(OL$Info, "Creating a scheme file")
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    writeSimpleDiffusionSchemeForSession(session, scheme)
    
    report(OL$Info, "Copying data and mask images")
    dataImage <- session$getImageByType("data", "diffusion")
    writeMriImageToCamino(dataImage, file.path(caminoDir,"data"))
    maskImage <- session$getImageByType("mask", "diffusion")
    writeMriImageToCamino(maskImage, file.path(caminoDir,"mask"))
    
    invisible (NULL)
}

createRadialDiffusivityMapForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    secondEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=2)
    thirdEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=3)
    radialDiffusivity <- newMriImageWithBinaryFunction(secondEigenvalue, thirdEigenvalue, function(x,y) (x+y)/2)
    
    writeImageFile(radialDiffusivity, session$getImageFileNameByType("radialdiff"))
}
