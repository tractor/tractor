getFileNameForStandardImage <- function (name = c("brain","white","grey","gray"), errorIfMissing = TRUE)
{
    if (is.null(.StandardBrainPath))
    {
        if (errorIfMissing)
            output(OL$Error, "Cannot find standard brain volumes")
        else
            return (NULL)
    }
    
    name <- match.arg(name)
    fileName <- switch(name, brain="avg152T1_brain",
                             white="avg152T1_white",
                             grey=,                     # drop-through
                             gray="avg152T1_gray")
    
    return (file.path(.StandardBrainPath, fileName))
}

getStandardImage <- function (name, errorIfMissing = TRUE)
{
    invisible (newMriImageFromFile(getFileNameForStandardImage(name)))
}
