superimposePng <- function (lowerFile, upperFile, newFile, maskFile = NULL)
{
    lowerFile <- ensureFileSuffix(lowerFile, "png")
    upperFile <- ensureFileSuffix(upperFile, "png")
    newFile <- ensureFileSuffix(newFile, "png")

    paramString <- paste(upperFile, lowerFile, sep=" ")
    if (is.character(maskFile))
    {
        maskFile <- ensureFileSuffix(maskFile, "png")
        paramString <- paste(paramString, maskFile, "+matte", sep=" ")
    }
    paramString <- paste(paramString, newFile, sep=" ")
    
    execute("composite", paramString, errorOnFail=TRUE)
}

interpolatePng <- function (oldFile, newFile, newDims, filter = "Mitchell")
{   
    oldFile <- ensureFileSuffix(oldFile, "png")
    newFile <- ensureFileSuffix(newFile, "png")
    
    dimsString <- paste(newDims[1], "x", newDims[2], "!", sep="")
    paramString <- paste("-filter", filter, "-resize", dimsString, oldFile, newFile, sep=" ")
    execute("convert", paramString, errorOnFail=TRUE)
}

writePng <- function (data, colourScale = 1, fileName = NULL, windowLimits = NULL)
{
    if (capabilities()["png"] == FALSE)
        output(OL$Error, "PNG output capability required")
    if (is.null(fileName))
        output(OL$Error, "File name must be specified")
        
    dims <- dim(data)
    if (length(dims) != 2)
        output(OL$Error, "Can only write 2D array data to a PNG file")
    
    fileName <- ensureFileSuffix(fileName, "png")
    scale <- getColourScale(colourScale)

    # Tracts acquire gaps if the width and length are not extended by 1
    # (don't know why!)
    png(fileName, bg=scale$background, width=dims[1]+1, height=dims[2]+1)
    par(mai=c(0,0,0,0))
    if (is.null(windowLimits))
        image(data, col=scale$colours, axes=FALSE, asp=dims[2]/dims[1])
    else
        image(data, col=scale$colours, axes=FALSE, asp=dims[2]/dims[1], zlim=sort(windowLimits))
    dev.off()
    
    invisible (fileName)
}
