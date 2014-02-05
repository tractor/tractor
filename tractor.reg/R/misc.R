isTemporaryFile <- function (fileName)
{
    return (regexpr(tempdir(), fileName, fixed=TRUE) == 1)
}

getImageAsFileName <- function (image, allowNull = FALSE, allowMgh = FALSE)
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
        }
        else    
            fileName <- image$getSource()
    }
    else if (is.character(image))
        fileName <- expandFileName(image)
    else
        report(OL$Error, "Specified image does not seem to be valid")
    
    if (!allowMgh && identifyImageFileNames(fileName)$format == "Mgh")
    {
        fileName <- threadSafeTempFile()
        writeImageFile(image, fileName, "NIFTI_GZ")
    }
    
    return (fileName)
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
