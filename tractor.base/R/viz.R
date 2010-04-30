interpolatePalette <- function (colours, n, ...)
{
    rampFunction <- colorRamp(colours)
    colourMatrix <- round(rampFunction(0:(n-1)/(n-1)))
    rgbStrings <- apply(colourMatrix, 1, function (x) sprintf("#%02X%02X%02X",x[1],x[2],x[3]))
    return (rgbStrings)
}

getColourScale <- function (n)
{
    if (is.list(n))
        return (n)
    else if (is.character(n))
        return (list(colours=c("black",n,n), background="black"))
    else
    {
        colours <- list(gray(0:99/99),
                        heat.colors(100),
                        rainbow(100, start=0.7, end=0.1),
                        interpolatePalette(c("#2166AC","#67A9CF","#D1E5F0","#F7F7F7","#FDDBC7","#EF8A62","#B2182B"), 100))  # ColorBrewer "RdBu" diverging palette
    
        background <- list("black",
                           "red",
                           "blue",
                           "#F7F7F7")
    
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

createSliceGraphic <- function (image, x = NA, y = NA, z = NA, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
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
        displayGraphic(slice, colourScale, add=add, windowLimits=windowLimits)
    else if (device == "png")
    {
        tempFile <- tempfile()
        pngDims <- round(abs(dims[!axisRelevance] * image$getVoxelDimensions()[!axisRelevance] * zoomFactor))
        writePng(slice, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

createProjectionGraphic <- function (image, axis, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    
    device <- match.arg(device)
    projection <- maximumIntensityProjection(image, axis)
    
    if (device == "internal")
        displayGraphic(projection, colourScale, add=add, windowLimits=windowLimits)
    else if (device == "png")
    {
        imageAxes <- !(1:3 %in% axis)
        tempFile <- tempfile()
        pngDims <- round(abs(image$getDimensions()[imageAxes] * image$getVoxelDimensions()[imageAxes] * zoomFactor))
        writePng(projection, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

createContactSheetGraphic <- function (image, axis, device = c("internal","png"), colourScale = 1, add = FALSE, file = NULL, zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
{
    if (!isMriImage(image))
        output(OL$Error, "The specified image is not an MriImage object")
    if (image$getDimensionality() != 3)
        output(OL$Error, "The \"createContactSheetGraphic\" function only handles 3D images")
    
    device <- match.arg(device)
    
    if (!is.null(clearance))
    {
        originalDims <- image$getDimensions()
        if (length(clearance) == 1)
        {
            clearance <- rep(clearance, image$getDimensionality())
            clearance[axis] <- 0
        }
        image <- newMriImageByTrimming(image, clearance)
        padding <- pmax(0, clearance - (originalDims - image$getDimensions()))
    }
    else
        padding <- rep(0, image$getDimensionality())
    
    dims <- image$getDimensions()
    if (is.null(nColumns))
        nColumns <- ceiling(sqrt(dims[axis]))
    nRows <- ceiling(dims[axis] / nColumns)
    imageAxes <- axis != 1:3
    padding <- padding[imageAxes]
    
    data <- matrix(NA, nrow=nColumns*(dims[imageAxes][1]+2*padding[1]), ncol=nRows*(dims[imageAxes][2]+2*padding[2]))
    for (i in seq_len(dims[axis]))
    {
        chunkRow <- (i-1) %/% nColumns + 1
        chunkCol <- (i-1) %% nColumns + 1
        rows <- ((chunkCol-1):chunkCol) * dims[imageAxes][1] + 1:0 + (2*chunkCol-1)*padding[1]
        cols <- ((chunkRow-1):chunkRow) * dims[imageAxes][2] + 1:0 + (2*chunkRow-1)*padding[2]
        data[rows[1]:rows[2],cols[1]:cols[2]] <- extractDataFromMriImage(image, axis, i)
    }
    
    if (device == "internal")
        displayGraphic(data, colourScale, add=add, windowLimits=windowLimits)
    else if (device == "png")
    {
        tempFile <- tempfile()
        pngDims <- round(abs(dim(data) * image$getVoxelDimensions()[imageAxes] * zoomFactor))
        writePng(data, colourScale, tempFile, windowLimits=windowLimits)
        interpolatePng(tempFile, file, pngDims, filter=filter)
        unlink(ensureFileSuffix(tempFile, "png"))
    }
}

createCombinedGraphics <- function (images, modes, colourScales, axes = 1:3, sliceLoc = NULL, device = c("internal","png"), alphaImages = NULL, prefix = "image", zoomFactor = 1, filter = "Mitchell", windowLimits = NULL, clearance = NULL, nColumns = NULL)
{
    if (!is.list(images) || !is.list(colourScales))
        output(OL$Error, "Images and colour scales must be given as lists")
    if (!is.null(alphaImages) && !is.list(alphaImages))
        output(OL$Error, "Alpha images must be specified in a list")
    if (!is.null(windowLimits) && !is.list(windowLimits))
        output(OL$Error, "Window limits must be specified in a list")
    if (!is.numeric(axes) || any(axes < 1 | axes > 3))
        output(OL$Error, "Projection axes must be specified as a combination of 1 (x), 2 (y) or 3 (z)")
    
    modes <- match.arg(modes, c("slice","projection","contact"), several.ok=TRUE)
    if (any(modes == "slice") && is.null(sliceLoc))
        output(OL$Error, "Slice location must be specified")
    if (any(modes == "contact") && !all(modes == "contact"))
        output(OL$Error, "Contact slice mode must be used for all graphics or none")
    
    device <- match.arg(device)
    
    nImages <- length(images)
    if (!all(c(length(modes),length(colourScales)) == nImages))
        output(OL$Error, "Lengths of 'images', 'modes' and 'colourScales' do not all match")
    if (!is.null(alphaImages) && length(alphaImages) != nImages)
        output(OL$Error, "Lengths of 'images' and 'alphaImages' do not match")
    if (!is.null(windowLimits) && length(windowLimits) != nImages)
        output(OL$Error, "Lengths of 'images' and 'windowLimits' do not match")
    
    if (device == "png")
    {
        projectionNames <- c("sagittal", "coronal", "axial")
        imageFiles <- tempfile(rep(prefix, 2*nImages))
        combinedFiles <- tempfile(rep(prefix, 2))
        
        for (axis in axes)
        {
            if (!is.null(sliceLoc))
            {
                currentSliceLoc <- sliceLoc
                currentSliceLoc[setdiff(1:3,axis)] <- NA
            }
            currentFile <- paste(prefix, projectionNames[axis], sep="_")
            
            for (i in seq_along(images))
            {
                if (modes[i] == "slice")
                {
                    createSliceGraphic(images[[i]], currentSliceLoc[1], currentSliceLoc[2], currentSliceLoc[3], device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]])
                    if (!is.null(alphaImages[[i]]))
                        createSliceGraphic(alphaImages[[i]], currentSliceLoc[1], currentSliceLoc[2], currentSliceLoc[3], device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter)
                }
                else if (modes[i] == "projection")
                {
                    createProjectionGraphic(images[[i]], axis, device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]])
                    if (!is.null(alphaImages[[i]]))
                        createProjectionGraphic(alphaImages[[i]], axis, device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter)
                }
                else
                {
                    createContactSheetGraphic(images[[i]], axis, device="png", colourScale=colourScales[[i]], file=imageFiles[2*i-1], zoomFactor=zoomFactor, filter=filter, windowLimits=windowLimits[[i]], clearance=clearance, nColumns=nColumns)
                    if (!is.null(alphaImages[[i]]))
                        createContactSheetGraphic(alphaImages[[i]], axis, device="png", colourScale=1, file=imageFiles[2*i], zoomFactor=zoomFactor, filter=filter, clearance=clearance, nColumns=nColumns)
                }
                
                if (i == 1)
                    file.copy(ensureFileSuffix(imageFiles[1],"png"), ensureFileSuffix(combinedFiles[1],"png"), overwrite=TRUE)
                else
                {
                    file.copy(ensureFileSuffix(combinedFiles[1],"png"), ensureFileSuffix(combinedFiles[2],"png"), overwrite=TRUE)
                    if (is.null(alphaImages[[i]]))
                        superimposePng(combinedFiles[2], imageFiles[2*i-1], combinedFiles[1])
                    else
                        superimposePng(combinedFiles[2], imageFiles[2*i-1], combinedFiles[1], imageFiles[2*i])
                }
            }
            
            file.copy(ensureFileSuffix(combinedFiles[1],"png"), ensureFileSuffix(currentFile,"png"), overwrite=TRUE)
            
            unlink(imageFiles)
            unlink(combinedFiles)
        }
    }
    else
        output(OL$Warning, "The 'createCombinedGraphics' function only supports the \"png\" device for now")
}
