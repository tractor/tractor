getFileNameForStandardImage <- function (name = c("brain","white","grey","gray"))
{
    if (is.null(.StandardBrainPath))
        output(OL$Error, "Cannot find standard brain volumes")
    
    name <- match.arg(name)
    fileName <- switch(name, brain="avg152T1_brain",
                             white="avg152T1_white",
                             grey=,                     # drop-through
                             gray="avg152T1_gray")
    
    return (file.path(.StandardBrainPath, fileName))
}

getStandardImage <- function (name)
{
    invisible (newMriImageFromFile(getFileNameForStandardImage(name)))
}
