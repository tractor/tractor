getFileNameForStandardImage <- function (name = c("brain","white","grey","gray","csf"), errorIfMissing = TRUE)
{
    if (is.null(.StandardBrainPath))
    {
        if (errorIfMissing)
            report(OL$Error, "Cannot find standard brain volumes")
        else
            return (NULL)
    }
    
    name <- match.arg(name)
    fileName <- switch(name, brain="brain",
                             white="white",
                             gray=,             # drop-through
                             grey="grey",
                             csf="csf")
    
    return (file.path(.StandardBrainPath, fileName))
}

getStandardImage <- function (name, errorIfMissing = TRUE)
{
    invisible (newMriImageFromFile(getFileNameForStandardImage(name)))
}
