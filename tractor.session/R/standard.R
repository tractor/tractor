getFileNameForStandardImage <- function (name = c("brain","face"), errorIfMissing = TRUE)
{
    if (is.null(.StandardBrainPath))
    {
        if (errorIfMissing)
            report(OL$Error, "Cannot find standard brain volumes")
        else
            return (NULL)
    }
    
    name <- match.arg(name)
    fileName <- switch(name, brain="brain", face="face_mask")
    
    return (file.path(.StandardBrainPath, fileName))
}

getStandardImage <- function (name, errorIfMissing = TRUE, ...)
{
    invisible (readImageFile(getFileNameForStandardImage(name), ...))
}
