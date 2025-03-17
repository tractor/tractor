#@args image file(s)
#@desc Display one or more image files. The viewer used will be taken from the TRACTOR_VIEWER environment variable if it is not specified explicitly, and will default to "tractor" if this is not set. Note that TractoR's internal viewer uses the R convention for voxel locations (starting at 1), whereas fslview and freeview use the FSL/C convention (starting at 0). For the internal viewer only, the fourth viewer panel can be used to show variation across a 4D series with respect to time or orientation. This wrapper script will automatically work around a problem with fslview and certain file datatypes.
#@group Visualisation
#@interactive TRUE
#@nohistory TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file(s)")
    
    requestedViewer <- getConfigVariable("Viewer", NULL, "character", validValues=c("tractor","fsleyes","fslview","freeview","mrview"))
    fixedWindow <- getConfigVariable("FixedWindow", TRUE, deprecated=TRUE)
    plotType <- getConfigVariable("PlotType", NULL, "character", validValues=c("none","time","orientation"))
    directionsFile <- getConfigVariable("DirectionsFile", NULL, "character")
    
    if (nArguments() > 1)
        lookupTable <- c("greyscale", rep("heat",nArguments()-1))
    else
        lookupTable <- "greyscale"
    
    directions <- NULL
    metadata <- lapply(Arguments, readImageFile, metadataOnly=TRUE)
    fileStems <- sapply(metadata, function(x) x$getSource())
    if (all(sapply(metadata, function(x) x$getDimensionality()) < 4))
        plotType <- "none"
    else if (!is.null(directionsFile))
        directions <- as.matrix(read.table(directionsFile))
    else if (any(file.exists(directionsFile <- ensureFileSuffix(fileStems, "dirs"))))
        directions <- as.matrix(read.table(directionsFile[file.exists(directionsFile)][1]))
    else
    {
        spaces <- sapply(fileStems, function(stem) {
            if (is.null(space <- guessSpace(stem,errorIfOutOfSession=FALSE)))
                return ("unknown")
            else
                return (groups(ore.search(":(\\w+)$", space)))
        })
        
        if (any(spaces == "diffusion") && file.exists(directionsFile <- file.path(dirname(fileStems[which(spaces=="diffusion")[1]]), "directions.txt")))
            directions <- as.matrix(read.table(directionsFile))
        else if (is.null(plotType) && any(spaces == "functional"))
        {
            report(OL$Info, "Image appears to be an fMRI time series; choosing time plot")
            plotType <- "time"
        }
    }
    
    if (is.null(plotType))
    {
        if (!is.null(directions))
        {
            report(OL$Info, "Image appears to be diffusion-weighted; choosing orientation plot")
            plotType <- "orientation"
        }
        else
        {
            report(OL$Info, "Defaulting to time plot")
            plotType <- "time"
        }
    }
    
    infoPanel <- switch(plotType, time=RNifti::timeSeriesPanel, orientation=polarPlotPanel(directions[,1:3],directions[,4]))
    viewerArguments <- c(as.list(Arguments), list(wait=TRUE,lookupTable=lookupTable,infoPanel=infoPanel))
    if (!is.null(requestedViewer))
        viewerArguments <- c(viewerArguments, list(viewer=requestedViewer))
    
    do.call(showImagesInViewer, viewerArguments)
    
    invisible(NULL)
}
