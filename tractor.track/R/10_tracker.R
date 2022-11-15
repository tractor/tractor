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
    }
))

createTracker <- function (model, mask, curvatureThreshold = 0.2, loopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, oneWay = FALSE)
{
    Tracker$new(model, mask, curvatureThreshold=curvatureThreshold, loopcheck=loopcheck, maxSteps=maxSteps, stepLength=stepLength, oneWay=oneWay)
}
