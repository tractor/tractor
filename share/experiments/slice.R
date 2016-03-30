#@args image file(s)
#@desc Create a 2D slice image from the specified Analyze/NIfTI/MGH volume. Exactly one of the X, Y and Z options must be specified, giving the location on the appropriate axis where the slice should be taken.

runExperiment <- function ()
{
    requireArguments("image file(s)")
    
    exist <- imageFileExists(Arguments)
    if (any(!exist))
        report(OL$Warning, "Image(s) #{implode(Arguments[!exist],sep=', ',finalSep=' and ')} do not exist")
    
    images <- lapply(Arguments[exist], readImageFile)
    dims <- sapply(images, dim, simplify="array")
    if (any(diff(t(dims)) != 0))
        report(OL$Error, "Images must have the same dimensions")
    
    outputPrefix <- paste(basename(images[[1]]$getSource()), "slice", sep="_")
    
    x <- getConfigVariable("X", NULL, "character")
    y <- getConfigVariable("Y", NULL, "character")
    z <- getConfigVariable("Z", NULL, "character")
    clearance <- getConfigVariable("Clearance", NULL, "integer")
    nColumns <- getConfigVariable("Columns", NULL, "integer")
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    colourScale <- getConfigVariable("ColourScale", "heat")
    projectOverlays <- getConfigVariable("ProjectOverlays", NULL, "logical")
    alpha <- getConfigVariable("Alpha", "binary", validValues=c("binary","linear","log"))
    zoomFactor <- getConfigVariable("ZoomFactor", 1)
    separate <- getConfigVariable("Separate", FALSE)
    
    colourScale <- switch(colourScale, greyscale=1L, grayscale=1L, heat=2L, rainbow=3L, "blue-red"=4L, red=5L, blue=6L, colourScale)
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        windowLimits <- lapply(seq_along(Arguments), function(i) {
            if (length(windowLimits) >= 2*i)
                windowLimits[c(2*i-1,2*i)]
            else
                NULL
        })
    }
    
    if (!is.null(clearance))
    {
        report(OL$Info, "Trimming images with #{clearance}-voxel clearance")
        images[[1]] <- trimMriImage(images[[1]], clearance)
        for (i in seq_len(length(images)-1))
            images[[i+1]] <- trimMriImage(images[[i+1]], indices=attr(images[[1]],"indices"))
    }
    
    resolveLocs <- function (axis, locs)
    {
        if (is.null(locs))
            return (integer(0))
        else if (locs == "all")
            result <- seq_len(dims[axis,1])
        else if (locs %~% "(\\d+)s")
            result <- seq(1, dims[axis,1], as.integer(ore.lastmatch()[,1]))
        else
            result <- splitAndConvertString(locs, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        
        if (!is.null(attr(images[[1]],"indices")))
            result <- na.omit(match(result, attr(images[[1]],"indices")[[axis]]))
        
        return (result)
    }
    
    report(OL$Info, "Creating graphics")
    tractor.base:::compositeImages(images, resolveLocs(1,x), resolveLocs(2,y), resolveLocs(3,z), colourScale=colourScale, projectOverlays=projectOverlays, alpha=alpha, prefix=outputPrefix, zoomFactor=zoomFactor, windowLimits=windowLimits, nColumns=nColumns, separate=separate)
}
