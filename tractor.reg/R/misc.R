isTemporaryFile <- function (fileName)
{
    return (regexpr(tempdir(), fileName, fixed=TRUE) == 1)
}

getImageAsFileName <- function (image, allowNull = FALSE)
{
    if (is.null(image))
    {
        if (allowNull)
            return (NULL)
        else
            report(OL$Error, "NULL is not allowed")
    }
    else if (is(image, "MriImage"))
    {
        if (image$isInternal())
        {
            fileName <- threadSafeTempFile()
            writeImageFile(image, fileName)
            return (fileName)
        }
        else    
            return (image$getSource())
    }
    else if (is.character(image))
        return (expandFileName(image))
    else
        report(OL$Error, "Specified image does not seem to be valid")
}

getImageAsObject <- function (image, ..., allowNull = FALSE)
{
    if (is.null(image))
    {
        if (allowNull)
            return (NULL)
        else
            report(OL$Error, "NULL is not allowed")
    }
    else if (is(image, "MriImage"))
        return (image)
    else if (is.character(image))
        return (readImageFile(image, ...))
    else
        report(OL$Error, "Specified image does not seem to be valid")
}
