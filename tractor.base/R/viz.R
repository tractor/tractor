getColourScale <- function (n)
{
    if (is.list(n))
        return (n)
    else if (is.character(n))
        return (list(colours=c("black",n,n), background="black"))
    else
    {
        colours <- list(gray(0:99/99),
                        heat.colors(100))
    
        background <- list("black",
                           "red")
    
        return (list(colours=colours[[n]], background=background[[n]]))
    }
}

maximumIntensityProjection <- function (image, axis)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    nDims <- image$getDimensionality()
    if (!(axis %in% 1:nDims))
        output(OL$Error, "Specified axis is not relevant for this image")
    
    planeAxes <- setdiff(1:nDims, axis)
    result <- apply(image$getData(), planeAxes, max)
    
    invisible(result)
}

createSliceGraphic <- function (image, x = NA, y = NA, z = NA, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() != 3)
        output(OL$Error, "The \"createSliceGraphic\" function only handles 3D images")
    
    device <- match.arg(device)
    
    dims <- image$getDimensions()
    axisShortNames <- c("x", "y", "z")
    axisRelevance <- !is.na(c(x, y, z))
    planeLoc <- c(x, y, z)[axisRelevance]
    
    if (length(which(axisRelevance)) != 1)
        output(OL$Error, "Exactly one of x, y and z must be specified")
    if (planeLoc < 1 || planeLoc > dims[axisRelevance])
        output(OL$Error, "Specified plane (", axisShortNames[axisRelevance], " = ", planeLoc, ") is out of bounds")
    
    slice <- extractDataFromMriImage(image, which(axisRelevance), planeLoc)
    
    if (device == "internal")
        displayGraphic(slice, colourScale, add=add)
    else if (device == "png")
        writePng(slice, colourScale, file)
}

createProjectionGraphic <- function (image, axis, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    device <- match.arg(device)
    projection <- maximumIntensityProjection(image, axis)
    
    if (device == "internal")
        displayGraphic(projection, colourScale, add=add)
    else if (device == "png")
        writePng(projection, colourScale, file)
}
