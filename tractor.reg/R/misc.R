isTemporaryFile <- function (fileName)
{
    return (regexpr(tempdir(), fileName, fixed=TRUE) == 1)
}

getImageAsFileName <- function (image, allowNull = TRUE, warnIfNotLas = FALSE)
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
    {
        fileName <- expandFileName(image)
        if (warnIfNotLas)
        {
            metadata <- readImageFile(fileName, metadataOnly=TRUE)
            if (!is.na(metadata$getStoredXformMatrix()[1,1]))
            {
                xform <- metadata$getStoredXformMatrix()
                diagonal <- diag(xform)[1:3]
                tolerance <- 1e-3 * max(abs(diagonal))
                if (any(diagonal*c(-1,1,1) < 0) || !equivalent(xform[1:3,1:3],diag(diagonal),tolerance=tolerance))
                    flag(OL$Warning, "NIfTI image is not stored in the LAS convention - problems may occur")
            }
        }
        return (fileName)
    }
    else
        report(OL$Error, "Specified image does not seem to be valid")
}

getImageAsObject <- function (image, allowNull = TRUE)
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
        return (readImageFile(image))
    else
        report(OL$Error, "Specified image does not seem to be valid")
}
