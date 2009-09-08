displayGraphic <- function (data, colourScale = 1, add = FALSE)
{
    dims <- dim(data)
    scale <- getColourScale(colourScale)
    
    if (add)
    {
        data <- replace(data, which(data==0), NA)
        image(data, col=scale$colours, add=TRUE)
    }
    else
    {
        oldPars <- par(mai=c(0,0,0,0), bg=scale$background)
        image(data, col=scale$colours, axes=FALSE, asp=dims[2]/dims[1])
        par(oldPars)
    }
}
