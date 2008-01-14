displayProbtrackResult <- function (probtrackResult, axes = 1:3, colourScale = 2, baseImage = "avf", ...)
{
    if (!is.numeric(axes) || any(axes < 1 | axes > 3))
        output(OL$Error, "Projection axes must be specified as a combination of 1 (x), 2 (y) or 3 (z)")
    
    if (!isMriImage(baseImage))
        baseImage <- probtrackResult$session$getImageByType(baseImage)
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    
    for (axis in axes)
    {
        seed <- probtrackResult$seed
        seed[setdiff(1:3,axis)] <- NA
        
        get(getOption("device"))()
        createSliceGraphic(baseImage, seed[1], seed[2], seed[3], device="internal")
        createProjectionGraphic(finalImage, axis, device="internal", colourScale=colourScale, add=TRUE)
    }
}

writePngsForResult <- function (probtrackResult, axes = 1:3, colourScale = 2, zoomFactor = 1, prefix = "tract", baseImage = "avf", showSeed = FALSE, ...)
{
    if (!is.numeric(axes) || any(axes < 1 | axes > 3))
        output(OL$Error, "Projection axes must be specified as a combination of 1 (x), 2 (y) or 3 (z)")
    
    projectionNames <- c("sagittal", "coronal", "axial")
    tmpFiles <- tempfile(rep(prefix, 3))
    
    if (!isMriImage(baseImage))
        baseImage <- probtrackResult$session$getImageByType(baseImage)
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    if (is.null(images$threshold))
        logFinalImage <- newMriImageWithSimpleFunction(finalImage, function (x) { ifelse(x == 0, 0, log(x)) }, newDataType=16)
    else
        logFinalImage <- newMriImageWithSimpleFunction(finalImage, function (x) { ifelse(x == 0, 0, log(x/images$threshold)) }, newDataType=16)
    
    imageDims <- baseImage$getDimensions()
    voxelDims <- baseImage$getVoxelDimensions()
    pngDims <- round(abs(imageDims * voxelDims * zoomFactor))
    
    if (showSeed)
        seedImage <- newMriImageWithData(generateImageDataForShape("cross",imageDims,centre=probtrackResult$seed,width=7), baseImage$getMetadata())
    
    for (axis in axes)
    {
        seed <- probtrackResult$seed
        seed[setdiff(1:3,axis)] <- NA
        currentDims <- pngDims[setdiff(1:3,axis)]
        currentFile <- paste(prefix, projectionNames[axis], sep="_")
        
        createSliceGraphic(baseImage, seed[1], seed[2], seed[3], device="png", file=tmpFiles[1], filter="Sinc")
        createProjectionGraphic(finalImage, axis, device="png", colourScale=colourScale, file=tmpFiles[2], filter="Sinc")
        createProjectionGraphic(logFinalImage, axis, device="png", colourScale=1, file=tmpFiles[3], filter="Sinc")
        
        if (showSeed)
        {
            moreTmpFiles <- tempfile(rep(prefix, 3))
            createProjectionGraphic(seedImage, axis, device="png", colourScale="green", file=moreTmpFiles[1], filter="Sinc")
            createProjectionGraphic(seedImage, axis, device="png", colourScale=1, file=moreTmpFiles[2], filter="Sinc")
            
            superimposePng(tmpFiles[1], tmpFiles[2], moreTmpFiles[3], tmpFiles[3])
            superimposePng(moreTmpFiles[3], moreTmpFiles[1], currentFile, moreTmpFiles[2])
            unlink(moreTmpFiles)
        }
        else
            superimposePng(tmpFiles[1], tmpFiles[2], currentFile, tmpFiles[3])

        unlink(tmpFiles)
    }
}

createWeightingAndMetricImagesForResult <- function (probtrackResult, type = c("prob","avf","fa","md"), mode = c("weighted","log","binary"), threshold = NULL)
{
    if (!is.null(probtrackResult$image))
        tractImage <- probtrackResult$image
    else if (!is.null(probtrackResult$fileName))
        tractImage <- newMriImageFromFile(probtrackResult$fileName)
    else
        output(OL$Error, "Cannot use a result containing no tract image or file name")
    
    type <- match.arg(type)
    
    if (type == "prob")
    {
        metricImage <- tractImage
        mode <- "binary"
    }
    else
    {
        metricImage <- probtrackResult$session$getImageByType(type)
        mode <- match.arg(mode)
    }

    if (mode == "weighted")
    {
        threshold <- NULL
        weightImage <- tractImage
    }
    else if (mode == "log")
    {
        threshold <- NULL
        weightImage <- newMriImageWithSimpleFunction(tractImage, function (x) { ifelse(x == 0, 0, log(x)) }, newDataType=16)
    }
    else if (mode == "binary")
    {
        threshold <- ifelse(is.null(threshold), 1, round(threshold * probtrackResult$nSamples))
        weightImage <- newMriImageWithSimpleFunction(tractImage, function (x) { ifelse(x > threshold, 1, 0) }, newDataType=2)
    }

    invisible (list(metric=metricImage, weight=weightImage, threshold=threshold))
}

calculateMetricForResult <- function (probtrackResult, ...)
{
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    metric <- sum(finalImage$getData()) / sum(images$weight$getData())
    return (metric)
}
