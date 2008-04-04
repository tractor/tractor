.FieldTract <- function (.image, .seed)
{
    if (!isMriImage(.image))
        output(OL$Error, "Specified image is not an MriImage object")
    if (!is.vector(.seed) || !is.numeric(.seed))
        output(OL$Error, "Seed point must be specified as a numeric vector")
    
    self <- list(
        getImage = function () { return (.image) },
        
        getSeedPoint = function () { return (.seed) }
    )
    
    class(self) <- c("tract.field", "list.object", "list")
    self <- inherit(self, .image)
    invisible (self)
}

isFieldTract <- function (object)
{
    return ("tract.field" %in% class(object))
}

deserialiseFieldTract <- function (file = NULL, object = NULL)
{
    if (is.null(object))
        object <- deserialiseListObject(file, raw=TRUE)
    
    if (isDeserialisable(object$image, "image.mri"))
        object$image <- deserialiseMriImage(object=object$image)
    else
        output(OL$Error, "Deserialised object contains no valid image")
    
    tract <- deserialiseListObject(NULL, object, .FieldTract)
    invisible (tract)
}

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
    
    tract <- .FieldTract(image, probtrackResult$seed)
    invisible (tract)
}

newFieldTractFromMriImage <- function (image, seed)
{
    tract <- .FieldTract(image, seed)
    invisible (tract)
}

newFieldTractByMasking <- function (tract, mask)
{
    newImage <- newMriImageByMasking(tract$getImage(), mask)
    newTract <- .FieldTract(newImage, tract$getSeedPoint())
    invisible (newTract)
}
