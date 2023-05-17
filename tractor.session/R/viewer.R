showImagesInViewer <- function (..., viewer = getOption("tractorViewer"), interactive = TRUE, wait = FALSE, lookupTable = NULL, opacity = NULL, infoPanel = NULL)
{
    viewer <- match.arg(viewer, .Viewers)
    imageList <- list(...)
    indexNames <- rep(list(NULL), length(imageList))
    
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
    lookupTableMappings <- list(tractor=list(Greyscale=1,grayscale=1,greyscale=1,"Red-Yellow"=2,heat=2,.default=tolower),
                                fsleyes=list(grayscale="greyscale",greyscale="greyscale",heat="red-yellow",.default=tolower),
                                fslview=list(grayscale="Greyscale",greyscale="Greyscale",heat="Red-Yellow",.default=capitalise),
                                freeview=list(Greyscale="grayscale",greyscale="grayscale","Red-Yellow"="heat",.default=tolower),
                                mrview=list(grayscale="Gray",greyscale="Gray",heat="Hot","red-yellow"="Hot",.default=capitalise))
    lookupTable <- lapply(lookupTable, function (l) {
        if (is.character(l) && l %in% names(lookupTableMappings[[viewer]]))
            return (lookupTableMappings[[viewer]][[l]])
        else if (is.character(l))
            return (lookupTableMappings[[viewer]]$.default(l))
        else
            return (l)
    })
    
    if (viewer == "tractor")
    {
        images <- lapply(imageList, function (image) {
            if (is.character(image))
                return (readImageFile(image))
            else if (is(image, "MriImage"))
                return (image)
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        })
        
        colourScales <- lapply(lookupTable, tractor.base::getColourScale)
        for (i in seq_along(images))
        {
            if (!images[[i]]$isInternal() && file.exists(ensureFileSuffix(images[[i]]$getSource(),"lut")))
            {
                regions <- read.table(ensureFileSuffix(images[[i]]$getSource(),"lut"), header=TRUE, stringsAsFactors=FALSE)
                imageRange <- range(images[[i]], na.rm=TRUE)
                regions <- subset(regions, index>=imageRange[1] & index<=imageRange[2])
                colours <- rep(NA, imageRange[2]-imageRange[1]+1)
                colours[regions$index - imageRange[1] + 1] <- regions$colour
                colourScales[[i]] <- list(background="#000000", colours=colours)
                indexNames[[i]] <- structure(regions$label, names=as.character(regions$index))
            }
        }
        
        if (!is.null(opacity))
        {
            for (i in seq_along(images))
                colourScales[[i]]$colours <- shades::opacity(colourScales[[i]]$colours, recycle(opacity))
        }
        
        if (is.null(infoPanel))
            infoPanel <- where(all(sapply(indexNames, is.null)), RNifti::defaultInfoPanel) %||% augmentedInfoPanel(indexNames)
        
        do.call(viewImages, list(images, interactive=interactive, colourScales=colourScales, infoPanel=infoPanel))
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
                
                if (viewer == "fslview" || viewer == "fsleyes")
                {
                    # fslview is fussy about data types, so write the image into Analyze format to avoid a crash if necessary
                    if (imageInfo$format == "Mgh" || (imageInfo$format == "Nifti" && RNifti::niftiHeader(imageInfo$imageFile)$datatype > 64))
                    {
                        dir.create(file.path(tempDir, i))
                        imageLoc <- file.path(tempDir, i, basename(imageInfo$fileStem))
                        imageInfo <- writeImageFile(readImageFile(imageInfo$imageFile), imageLoc, fileType="NIFTI_GZ")
                    }
                }
            }
            else if (is(imageList[[i]], "MriImage"))
            {
                dir.create(file.path(tempDir, i))
                imageLoc <- file.path(tempDir, i, basename(imageList[[i]]$getSource()))
                imageInfo <- writeImageFile(imageList[[i]], imageLoc, fileType="NIFTI_GZ")
            }
            else
                report(OL$Error, "Images must be specified as MriImage objects or file names")
        
            return (imageInfo$imageFile)
        })
        
        if (viewer == "fsleyes")
            showImagesInFsleyes(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "fslview")
            showImagesInFslview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "freeview")
            showImagesInFreeview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        else if (viewer == "mrview")
            showImagesInMrview(imageFileNames, wait=wait, lookupTable=unlist(lookupTable), opacity=opacity)
        
        # If we're not waiting for the program to exit, we can't delete the images yet
        if (wait)
            unlink(tempDir, recursive=TRUE)
    }
}
