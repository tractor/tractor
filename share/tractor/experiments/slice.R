#@args image file(s)
#@desc Create one or more composited slice images from the specified Analyze/NIfTI/MGH volumes. At least one of the X, Y and Z options must be specified, giving the location(s) on the appropriate axis where the slice should be taken: values for these variables can be a one or more literal integers, comma separated, or an integer followed by "s" for every nth slice, or "all" for all slices, "max" for the slice containing the maximum total image intensity (in the topmost image), or "peak" for the peak intensity location. All slices will be tiled into one big image unless Separate:true is given.
#@example # Overlay a tract visitation map on the corresponding FA map
#@example tractor slice session@FA tract X:max Y:max Z:max Alpha:log
#@example # A contact-sheet style version
#@example tractor slice session@FA tract Z:all Clearance:4 Alpha:log
#@group Visualisation

runExperiment <- function ()
{
    requireArguments("image file(s)")
    
    x <- getConfigVariable("X", NULL, "character")
    y <- getConfigVariable("Y", NULL, "character")
    z <- getConfigVariable("Z", NULL, "character")
    volume <- getConfigVariable("Volume", NULL, "integer")
    clearance <- getConfigVariable("Clearance", NULL, "character")
    nColumns <- getConfigVariable("Columns", NULL, "integer")
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    colourScales <- getConfigVariable("ColourScale", "heat")
    projectOverlays <- getConfigVariable("ProjectOverlays", NULL, "logical")
    alpha <- getConfigVariable("Alpha", "binary", validValues=c("binary","linear","log"))
    interpolation <- getConfigVariable("Interpolation", "spline", validValues=c("nearestneighbour","trilinear","spline"))
    zoomFactor <- getConfigVariable("ZoomFactor", 1)
    clip <- getConfigVariable("Clip", TRUE)
    separate <- getConfigVariable("Separate", FALSE)
    outputPrefix <- getConfigVariable("GraphicName", "slices")
    
    if (is.null(x) && is.null(y) && is.null(z))
        report(OL$Error, "At least one of X, Y and Z should be specified")
    
    exist <- imageFileExists(Arguments)
    if (any(!exist))
        report(OL$Warning, "Image(s) #{implode(Arguments[!exist],sep=', ',finalSep=' and ')} do not exist")
    
    images <- lapply(Arguments[exist], readImageFile, volumes=volume)
    dims <- sapply(images, dim, simplify="array")
    if (any(diff(t(dims)) != 0))
        report(OL$Error, "Images must have the same dimensions")
    
    colourScales <- splitAndConvertString(colourScales, ",", fixed=TRUE)
    colourScales <- lapply(colourScales, function(scale) switch(scale, greyscale=1L, grayscale=1L, heat=2L, rainbow=3L, "blue-red"=4L, reds=5L, blues=6L, "yellow-red"=7L, viridis=8L, scale))
    
    interpolationKernel <- switch(interpolation, nearestneighbour=mmand::boxKernel(), trilinear=mmand::triangleKernel(), spline=mmand::mnKernel())
    
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
        clearance <- splitAndConvertString(clearance, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
        images[[1]] <- trimMriImage(images[[1]], clearance)
        for (i in seq_len(length(images)-1))
            images[[i+1]] <- trimMriImage(images[[i+1]], indices=attr(images[[1]],"indices"))
        dims <- sapply(images, dim, simplify="array")
    }
    
    resolveLocs <- function (axis, locs)
    {
        if (is.null(locs))
            return (integer(0))
        else if (locs == "all")
            result <- seq_len(dims[axis,1])
        else if (locs == "max")
            result <- which.max(images[[length(images)]]$apply(axis,sum,na.rm=TRUE))
        else if (locs == "peak")
            result <- which.max(images[[length(images)]]$apply(axis,max,na.rm=TRUE))
        else if (locs %~% "(\\d+)s")
            result <- seq(1, dims[axis,1], as.integer(ore.lastmatch()[,1]))
        else
        {
            result <- splitAndConvertString(locs, ",", "integer", fixed=TRUE, errorIfInvalid=TRUE)
            if (!is.null(attr(images[[1]],"indices")))
                result <- na.omit(match(result, attr(images[[1]],"indices")[[axis]]))
        }
        
        report(OL$Verbose, "Using #{LETTERS[24:26][axis]} slice(s) #{implode(as.integer(result),', ',' and ',ranges=TRUE)}")
        
        return (result)
    }
    
    report(OL$Info, "Creating graphics")
    tractor.base:::compositeImages(images, resolveLocs(1,x), resolveLocs(2,y), resolveLocs(3,z), colourScales=colourScales, projectOverlays=projectOverlays, alpha=alpha, prefix=outputPrefix, zoomFactor=zoomFactor, windowLimits=windowLimits, nColumns=nColumns, separate=separate, clip=clip, interpolationKernel=interpolationKernel)
}
