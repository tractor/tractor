# NB: The underlying C++ class is not thread-safe, so a Tracker object should not be run multiple times concurrently
Tracker <- setRefClass("Tracker", fields=list(model="DiffusionModel",pointer="externalptr"), methods=list(
    initialize = function (model = nilModel(), mask = NULL, curvatureThreshold = 0.2, loopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, oneWay = FALSE, ...)
    {
        if (nilModel(model))
            ptr <- nilPointer()
        else
            ptr <- .Call("createTracker", model$getPointer(), mask, maxSteps, stepLength, curvatureThreshold, loopcheck, oneWay, PACKAGE="tractor.track")
        
        initFields(model=model, pointer=ptr)
    },
    
    getModel = function () { return (model) },
    
    setTargets = function (image, indices = NULL, labels = NULL, terminate = FALSE)
    {
        if (is.list(image))
        {
            indices <- indices %||% image$indices
            labels <- labels %||% image$labels
            image <- image$image
        }
        
        if (is.character(image) && length(image) == 1 && identifyImageFileNames(image)$format == "Mrtrix")
        {
            path <- threadSafeTempFile("target")
            image <- writeImageFile(image, path, "NIFTI")$fileStem
        }
        
        .Call("setTrackerTargets", pointer, list(image=image,indices=indices,labels=labels), terminate, PACKAGE="tractor.track")
        return (.self)
    },
    
    run = function (seeds, count, basename = threadSafeTempFile(), profileFun = NULL, requireMap = TRUE, requireStreamlines = FALSE, requireMedian = FALSE, terminateAtTargets = FALSE, jitter = TRUE)
    {
        if (nilModel(model))
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

createTracker <- function (model, mask, curvatureThreshold = 0.2, loopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, oneWay = FALSE)
{
    Tracker$new(model, mask, curvatureThreshold=curvatureThreshold, loopcheck=loopcheck, maxSteps=maxSteps, stepLength=stepLength, oneWay=oneWay)
}
