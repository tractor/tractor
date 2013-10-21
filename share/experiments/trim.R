#@args image file, volume numbers
#@desc Trim an image, keeping or discarding the specified volumes (depending on the value of the Mode option), with 1 being the first volume. Note that the original image will be OVERWRITTEN by the new, smaller image.

runExperiment <- function ()
{
    requireArguments("image file", "volume numbers")
    
    mode <- getConfigVariable("Mode", "keep", validValues=c("keep","discard"))
    
    metadata <- readImageFile(Arguments[1])
    if (metadata$getDimensionality() != 4)
        report(OL$Error, "The specified image is not four-dimensional")
    
    volumes <- splitAndConvertString(Arguments[2], ",", "integer", fixed=TRUE, errorIfInvalid=TRUE, allowRanges=TRUE)
    if (!all(volumes %in% seq_len(metadata$getDimensions()[4])))
        report(OL$Error, "The specified volumes numbers are not all within the image")
    
    if (mode == "discard")
        volumes <- setdiff(seq_len(metadata$getDimensions()[4]), volumes)
    
    image <- readImageFile(Arguments[1], volumes=volumes)
    writeImageFile(image, Arguments[1])
    
    invisible(NULL)
}
