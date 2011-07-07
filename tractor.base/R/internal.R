displayGraphic <- function (data, colourScale = 1, add = FALSE, windowLimits = NULL)
{
    dims <- dim(data)
    scale <- getColourScale(colourScale)
    
    if (!is.null(windowLimits))
    {
        data[data < min(windowLimits)] <- min(windowLimits)
        data[data > max(windowLimits)] <- max(windowLimits)
    }
    
    if (add)
    {
        data <- replace(data, which(data==0), NA)
        if (is.null(windowLimits))
            image(data, col=scale$colours, add=TRUE)
        else
            image(data, col=scale$colours, add=TRUE, zlim=sort(windowLimits))
    }
    else
    {
        oldPars <- par(mai=c(0,0,0,0), bg=scale$background)
        if (is.null(windowLimits))
            image(data, col=scale$colours, axes=FALSE, asp=dims[2]/dims[1])
        else
            image(data, col=scale$colours, axes=FALSE, asp=dims[2]/dims[1], zlim=sort(windowLimits))
        par(oldPars)
    }
}
