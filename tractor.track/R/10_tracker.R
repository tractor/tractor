# NB: The underlying C++ class is not thread-safe, so a Tracker object should not be run multiple times concurrently
Tracker <- setRefClass("Tracker", fields=list(model="DiffusionModel",maskPath="character",targetPath="character",options="list",filters="list"), methods=list(
    initialize = function (model = nilModel(), maskPath = character(0), targetPath = character(0), curvatureThreshold = 0.2, useLoopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, rightwardsVector = NULL, ...)
    {
        object <- initFields(model=model, options=list(curvatureThreshold=curvatureThreshold, useLoopcheck=useLoopcheck, maxSteps=maxSteps, stepLength=stepLength, rightwardsVector=rightwardsVector), filters=list(minLength=0, minTargetHits=0L))
        
        object$setMask(maskPath)
        object$setTargets(targetPath)
        
        invisible(object)
    },
    
    setFilters = function (...)
    {
        args <- list(...)
        indices <- which(names(args) != "")
        if (length(indices) > 0)
            .self$filters[names(args)[indices]] <- args[indices]
        invisible(filters)
    },
    
    setMask = function (image)
    {
        if (is.null(image) || length(image) == 0)
            .self$maskPath <- character(0)
        else if (is.character(image) && length(image) == 1)
            .self$maskPath <- identifyImageFileNames(image)$fileStem
        else if (is(image, "MriImage"))
        {
            .self$maskPath <- threadSafeTempFile("mask")
            writeImageFile(image, .self$maskPath)
        }
        else
            report(OL$Error, "Mask should be specified as a file name or MriImage object")
    },
    
    setOptions = function (...)
    {
        args <- list(...)
        indices <- which(names(args) != "")
        if (length(indices) > 0)
            .self$options[names(args)[indices]] <- args[indices]
        invisible(options)
    },
    
    setTargets = function (image)
    {
        if (is.null(image) || length(image) == 0)
            .self$targetPath <- character(0)
        else if (is.character(image) && length(image) == 1)
            .self$targetPath <- identifyImageFileNames(image)$fileStem
        else if (is(image, "MriImage"))
        {
            .self$targetPath <- threadSafeTempFile("target")
            writeImageFile(image, .self$targetPath)
        }
        else
            report(OL$Error, "Targets should be specified as a file name or MriImage object")
    },
    
    run = function (seeds, count, basename = threadSafeTempFile(), profileFun = NULL, requireMap = TRUE, requireStreamlines = FALSE, terminateAtTargets = FALSE, terminateOutsideMask = FALSE, mustLeaveMask = FALSE, jitter = FALSE)
    {
        if (is.nilModel(model))
            report(OL$Error, "No diffusion model has been specified")
        if (length(maskPath) == 0)
            report(OL$Error, "No tracking mask has been specfied")
        
        targetsPath <- mapPath <- streamlinePath <- NULL
        if (length(.self$targetPath) == 1)
            targetsPath <- .self$targetPath
        if (requireMap)
            mapPath <- basename
        if (requireStreamlines)
            streamlinePath <- basename
        
        seeds <- promote(seeds, byrow=TRUE)
        
        nRetained <- .Call("track", model$getPointer(), seeds, as.integer(count), maskPath, targetsPath, options$rightwardsVector, as.integer(options$maxSteps), as.double(options$stepLength), as.double(options$curvatureThreshold), isTRUE(options$useLoopcheck), isTRUE(terminateAtTargets), as.integer(filters$minTargetHits), as.numeric(filters$minLength), isTRUE(terminateOutsideMask), isTRUE(mustLeaveMask), isTRUE(jitter), mapPath, streamlinePath, profileFun, 0L, PACKAGE="tractor.track")
        
        if (nRetained < nrow(seeds) * count)
            report(OL$Info, "#{nRetained} streamlines (#{signif(nRetained/(nrow(seeds)*count)*100,3)}%) were retained after filtering")
        
        return (basename)
    }
))
