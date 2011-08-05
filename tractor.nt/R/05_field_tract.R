FieldTract <- setRefClass("FieldTract", contains="MriImage", fields=list(seed="numeric"), methods=list(
    initialize = function (seed = NULL, image = NULL, ...)
    {
        if (!is.null(image))
            import(image, "MriImage")
        else
            callSuper(...)
        return (initFields(seed=as.numeric(seed)))
    },
    
    getImage = function () { return (export("MriImage")) },
        
    getSeedPoint = function () { return (seed) }
))

newFieldTractFromProbtrack <- function (..., threshold=NA)
{
    probtrackResult <- runProbtrackWithSession(..., requireImage=TRUE)
    if (is.na(threshold))
        image <- probtrackResult$image
    else
    {
        absoluteThreshold <- threshold * probtrackResult$nSamples
        thresholdFunction <- function (x) { return (x * (x >= absoluteThreshold)) }
        image <- newMriImageWithSimpleFunction(probtrackResult$image, thresholdFunction)
    }
    
    tract <- FieldTract$new(probtrackResult$seed, image)
    invisible (tract)
}

newFieldTractFromMriImage <- function (image, seed)
{
    tract <- FieldTract$new(seed, image)
    invisible (tract)
}

newFieldTractByMasking <- function (tract, mask)
{
    newImage <- newMriImageByMasking(tract$getImage(), mask)
    newTract <- FieldTract$new(tract$getSeedPoint(), newImage)
    invisible (newTract)
}
