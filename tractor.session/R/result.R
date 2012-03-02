getSeedCentrePointForResult <- function (probtrackResult)
{
    seedPoints <- promote(probtrackResult$seeds, byrow=TRUE)
    seedCentre <- round(apply(seedPoints, 2, median))
    return (seedCentre)
}

displayProbtrackResult <- function (probtrackResult, axes = 1:3, colourScale = 2, baseImage = "fa", ...)
{
    if (!is.numeric(axes) || any(axes < 1 | axes > 3))
        report(OL$Error, "Projection axes must be specified as a combination of 1 (x), 2 (y) or 3 (z)")
    
    if (!is(baseImage, "MriImage"))
        baseImage <- probtrackResult$session$getImageByType(baseImage, "diffusion")
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    
    seedCentre <- getSeedCentrePointForResult(probtrackResult)
    
    for (axis in axes)
    {
        seed <- seedCentre
        seed[setdiff(1:3,axis)] <- NA
        
        createSliceGraphic(baseImage, seed[1], seed[2], seed[3], device="internal")
        createProjectionGraphic(finalImage, axis, device="internal", colourScale=colourScale, add=TRUE)
    }
}

writePngsForResult <- function (probtrackResult, axes = 1:3, colourScale = 2, zoomFactor = 1, prefix = "tract", baseImage = "fa", showSeed = FALSE, ...)
{
    if (!is(baseImage, "MriImage"))
        baseImage <- probtrackResult$session$getImageByType(baseImage, "diffusion")
    imageDims <- baseImage$getDimensions()
    
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    if (is.null(images$threshold))
        logFinalImage <- newMriImageWithSimpleFunction(finalImage, function (x) { ifelse(x == 0, 0, log(x)) }, newDataType=getDataTypeByNiftiCode(16))
    else
        logFinalImage <- newMriImageWithSimpleFunction(finalImage, function (x) { ifelse(x == 0, 0, log(x/images$threshold)) }, newDataType=getDataTypeByNiftiCode(16))
    
    seedCentre <- getSeedCentrePointForResult(probtrackResult)
    
    if (!showSeed)
        createCombinedGraphics(list(baseImage,finalImage), c("s","p"), list(1,colourScale), axes=axes, sliceLoc=seedCentre, device="png", alphaImages=list(NULL,logFinalImage), prefix=prefix, zoomFactor=zoomFactor, filter="Sinc")
    else
    {
        seedImage <- newMriImageWithData(generateImageDataForShape("cross",imageDims,centre=seedCentre,width=7), baseImage$getMetadata())
        createCombinedGraphics(list(baseImage,finalImage,seedImage), c("s","p","p"), list(1,colourScale,"green"), axes=axes, sliceLoc=seedCentre, device="png", alphaImages=list(NULL,logFinalImage,seedImage), prefix=prefix, zoomFactor=zoomFactor, filter="Sinc")
    }
}

createWeightingAndMetricImages <- function (image, session = NULL, type = c("weight","fa","md","axialdiff","radialdiff"), mode = c("weighted","log","binary"), threshold = NULL)
{
    type <- match.arg(type)
    
    if (type == "weight")
    {
        metricImage <- image
        mode <- "binary"
    }
    else if (is.null(session))
        report(OL$Error, "A session must be specified for metric ", toupper(type))
    else
    {
        metricImage <- session$getImageByType(type, "diffusion")
        mode <- match.arg(mode)
    }

    if (mode == "weighted")
    {
        threshold <- NULL
        weightImage <- image
    }
    else if (mode == "log")
    {
        threshold <- NULL
        weightImage <- newMriImageWithSimpleFunction(image, function (x) { ifelse(x == 0, 0, log(x)) }, newDataType=getDataTypeByNiftiCode(16))
    }
    else if (mode == "binary")
    {
        threshold <- ifelse(is.null(threshold), 1, threshold)
        weightImage <- newMriImageWithSimpleFunction(image, function (x) { ifelse(x > threshold, 1, 0) }, newDataType=getDataTypeByNiftiCode(2))
    }

    invisible (list(metric=metricImage, weight=weightImage, threshold=threshold))
}

createWeightingAndMetricImagesForResult <- function (probtrackResult, threshold = NULL, ...)
{
    if (!is.null(probtrackResult$image))
        tractImage <- probtrackResult$image
    else if (!is.null(probtrackResult$fileName))
        tractImage <- newMriImageFromFile(probtrackResult$fileName)
    else
        report(OL$Error, "Cannot use a result containing no tract image or file name")
    
    if (!is.null(threshold))
        threshold <- threshold * probtrackResult$nSamples
    
    images <- createWeightingAndMetricImages(tractImage, probtrackResult$session, ..., threshold=threshold)
    invisible (images)
}

calculateMetricForResult <- function (probtrackResult, ...)
{
    images <- createWeightingAndMetricImagesForResult(probtrackResult, ...)
    finalImage <- newMriImageWithBinaryFunction(images$metric, images$weight, "*")
    metric <- sum(finalImage$getData()) / sum(images$weight$getData())
    return (metric)
}
