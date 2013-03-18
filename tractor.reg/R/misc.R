getImageAsFileName <- function (image)
{
    if (is.null(image))
        return (NULL)
    else if (is(image, "MriImage"))
        return (image$getSource())
    else if (is.character(image))
        return (expandFileName(image))
    else
        report(OL$Error, "Specified image does not seem to be valid")
}

getImageAsObject <- function (image)
{
    if (is.null(image))
        return (NULL)
    else if (is(image, "MriImage"))
        return (image)
    else if (is.character(image))
        return (readImageFile(image))
    else
        report(OL$Error, "Specified image does not seem to be valid")
}
