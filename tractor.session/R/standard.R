getFileNameForStandardImage <- function (name = "brain", errorIfMissing = TRUE)
{
    if (is.null(.StandardBrainPath))
    {
        if (errorIfMissing)
            report(OL$Error, "Cannot find standard brain volumes")
        else
            return (NULL)
    }
    
    # There used to be other options, but for now they have gone
    name <- match.arg(name)
    fileName <- switch(name, brain="brain")
    
    return (file.path(.StandardBrainPath, fileName))
}

getStandardImage <- function (name, errorIfMissing = TRUE)
{
    invisible (readImageFile(getFileNameForStandardImage(name)))
}
