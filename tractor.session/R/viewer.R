showImagesInViewer <- function (..., viewer = getOption("tractorViewer"), interactive = TRUE, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    viewer <- match.arg(viewer, .Viewers)
    imageList <- list(...)
    
    if (is.null(lookupTable))
        lookupTable <- rep(list("greyscale"), length(imageList))
    else if (is.character(lookupTable))
        lookupTable <- rep(as.list(lookupTable), length.out=length(imageList))
    else if (!is.list(lookupTable))
        lookupTable <- rep(list(lookupTable), length(imageList))
    else
        lookupTable <- rep(lookupTable, length.out=length(imageList))
    
    if (!is.null(opacity) && any(opacity < 0 | opacity > 1))
        report(OL$Error, "Opacity values must be between 0 and 1")
    
    # Unmatched lookup table strings are passed through the function at the end of each list
    capitalise <- function (str) paste(toupper(substr(str,1,1)), tolower(substr(str,2,max(nchar(str)))), sep="")
    lookupTableMappings <- list(internal=list(Greyscale=1,grayscale=1,greyscale=1,"Red-Yellow"=2,heat=2,.default=tolower),
                                fslview=list(grayscale="Greyscale",greyscale="Greyscale",heat="Red-Yellow",.default=capitalise),
                                freeview=list(Greyscale="grayscale",greyscale="grayscale","Red-Yellow"="heat",.default=tolower))
    lookupTable <- lapply(lookupTable, function (l) {
        if (is.character(l) && l %in% names(lookupTableMappings[[viewer]]))
            return (lookupTableMappings[[viewer]][[l]])
        else if (is.character(l))
            return (lookupTableMappings[[viewer]]$.default(l))
        else
            return (l)
    })
    
    if (viewer == "internal")
    {
        images <- lapply(imageList, function (image) {
            if (is.character(image))
                return (newMriImageFromFile(image))
            else if (is(image, "MriImage"))
                return (image)
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        })
        
        colourScales <- lapply(lookupTable, tractor.base:::getColourScale)
        if (!is.null(opacity))
        {
            opacity <- rep(opacity, length.out=length(images))
            for (i in seq_along(images))
            {
                colourScales[[i]]$colours <- rgb(t(col2rgb(colourScales[[i]]$colours)), alpha=round(opacity[i]*255), max=255)
                colourScales[[i]]$background <- rgb(t(col2rgb(colourScales[[i]]$background)), alpha=round(opacity[i]*255), max=255)
            }
        }
        
        viewImages(images, interactive=interactive, colourScales=colourScales)
    }
    else
    {
        tempDir <- threadSafeTempFile()
        if (!file.exists(tempDir))
            dir.create(tempDir)
    
        imageFileNames <- lapply(seq_along(imageList), function (i) {
            if (is.character(imageList[[i]]))
            {
                imageInfo <- identifyImageFileNames(imageList[[i]], errorIfMissing=FALSE)
                if (is.null(imageInfo))
                {
                    report(OL$Warning, "Image file \"", imageList[[i]], "\" does not exist")
                    return(NULL)
                }
                
                if (viewer == "fslview")
                {
                    metadata <- newMriImageMetadataFromFile(imageInfo$imageFile)
                    typeCode <- getNiftiCodeForDataType(metadata$getDataType())
            
                    # fslview is fussy about data types, so write the image into Analyze format to avoid a crash if necessary
                    if (imageInfo$format == "Mgh" || is.null(typeCode) || typeCode > 64)
                    {
                        dir.create(file.path(tempDir, i))
                        imageLoc <- file.path(tempDir, i, basename(metadata$getSource()))
                        imageInfo <- writeMriImageToFile(newMriImageFromFile(imageInfo$imageFile), imageLoc, fileType="ANALYZE_GZ")
                    }
                }
            }
            else if (is(imageList[[i]], "MriImage"))
            {
                dir.create(file.path(tempDir, i))
                imageLoc <- file.path(tempDir, i, basename(imageList[[i]]$getSource()))
                imageInfo <- writeMriImageToFile(imageList[[i]], imageLoc, fileType="ANALYZE_GZ")
            }
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        
            return (imageInfo$imageFile)
        })
        
        if (viewer == "fslview")
            showImagesInFslview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "freeview")
            showImagesInFreeview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        
        # If we're not waiting for the program to exit, we can't delete the images yet
        if (wait)
            unlink(tempDir, recursive=TRUE)
    }
}
