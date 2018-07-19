# NB: The underlying C++ class is not thread-safe, so a Tracker object should not be run multiple times concurrently
Tracker <- setRefClass("Tracker", fields=list(model="DiffusionModel",maskPath="character",targetInfo="list",options="list",filters="list"), methods=list(
    initialize = function (model = nilModel(), maskPath = character(0), targetInfo = list(), curvatureThreshold = 0.2, useLoopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, rightwardsVector = NULL, oneWay = FALSE, ...)
    {
        object <- initFields(model=model, options=list(curvatureThreshold=curvatureThreshold, useLoopcheck=useLoopcheck, maxSteps=maxSteps, stepLength=stepLength, rightwardsVector=rightwardsVector, oneWay=oneWay), filters=list(minLength=0, maxLength=Inf, minTargetHits=0L))
        
        object$setMask(maskPath)
        object$setTargets(targetInfo)
        
        invisible(object)
    },
    
    getModel = function () { return (model) },
    
    setFilters = function (...)
    {
        args <- list(...)
        indices <- which(names(args) != "")
        if (length(indices) > 0)
            .self$filters[names(args)[indices]] <- args[indices]
        return (.self)
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
        return (.self)
    },
    
    setOptions = function (...)
    {
        args <- list(...)
        indices <- which(names(args) != "")
        if (length(indices) > 0)
            .self$options[names(args)[indices]] <- args[indices]
        return (.self)
    },
    
    setTargets = function (image, indices = NULL, labels = NULL)
    {
        if (is.list(image))
        {
            if (is.null(indices))
                indices <- image$indices
            if (is.null(labels))
                labels <- image$labels
            image <- image$image
        }
        
        path <- NULL
        if (is.character(image) && length(image) == 1)
            path <- identifyImageFileNames(image)$fileStem
        else if (is(image, "MriImage"))
        {
            path <- threadSafeTempFile("target")
            writeImageFile(image, path)
        }
        else if (!is.null(image) && length(image) > 0)
            report(OL$Error, "Targets should be specified as a file name or MriImage object")
        
        if (length(indices) != length(labels))
            report(OL$Error, "Index and label vectors should have the same length")
        
        .self$targetInfo <- list(path=path, indices=indices, labels=labels)
        return (.self)
    },
    
    run = function (seeds, count, basename = threadSafeTempFile(), profileFun = NULL, requireMap = TRUE, requireStreamlines = FALSE, requireMedian = FALSE, terminateAtTargets = FALSE, jitter = TRUE)
    {
        if (is.nilModel(model))
            report(OL$Error, "No diffusion model has been specified")
        if (length(maskPath) == 0)
            report(OL$Error, "No tracking mask has been specfied")
        
        mapPath <- streamlinePath <- medianPath <- NULL
        if (requireMap)
            mapPath <- ensureFileSuffix(basename, tractor.base:::.FileTypes$headerSuffixes[tractor.base:::.FileTypes$typeNames == getOption("tractorFileType")])
        if (requireStreamlines)
            streamlinePath <- basename
        if (requireMedian)
            medianPath <- paste(basename, "median", sep="_")
        
        seeds <- promote(seeds, byrow=TRUE)
        
        nRetained <- .Call("track", model$getPointer(), seeds, as.integer(count), maskPath, .self$targetInfo, options$rightwardsVector, as.integer(options$maxSteps), as.double(options$stepLength), as.double(options$curvatureThreshold), isTRUE(options$useLoopcheck), isTRUE(options$oneWay), isTRUE(terminateAtTargets), as.integer(filters$minTargetHits), as.numeric(filters$minLength), as.numeric(filters$maxLength), isTRUE(jitter), mapPath, streamlinePath, medianPath, profileFun, 0L, PACKAGE="tractor.track")
        
        if (nRetained < nrow(seeds) * count)
            report(OL$Info, "#{nRetained} streamlines (#{signif(nRetained/(nrow(seeds)*count)*100,3)}%) were retained after filtering")
        
        return (basename)
    }
))
