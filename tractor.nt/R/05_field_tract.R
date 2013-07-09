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
