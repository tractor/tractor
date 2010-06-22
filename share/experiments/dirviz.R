#@desc Visualise major fibre directions in one slice of a diffusion MRI data set. The slice is selected by setting one of X, Y or Z. The source of the direction information can be "tensor" (in which case there is only one major direction per voxel) or "bedpost". Stage 4 or 5 of the "preproc" script, respectively, must have already been run on the specified session. Directions will not be shown for components whose FA (for Source:tensor) or volume fraction (for Source:bedpost) are below the specified ThresholdLevel. Direction lengths are scaled by the corresponding FA or volume fraction values unless ScaleComponents:false is given.
#@args session directory
#@interactive TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- newSessionFromDirectory(Arguments[1])
    
    if (!session$isPreprocessed())
        output(OL$Error, "This session is not yet fully preprocessed")
    
    x <- getWithDefault("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getWithDefault("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getWithDefault("Z", NA, "numeric", errorIfInvalid=TRUE)
    pointType <- getWithDefault("PointType", NULL, "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE, errorIfMissing=TRUE)
    source <- getWithDefault("Source", "bedpost", validValues=c("bedpost","tensor"))
    thresholdLevel <- getWithDefault("ThresholdLevel", 0.05)
    windowLimits <- getWithDefault("WindowLimits", NULL, "character")
    scaleComponents <- getWithDefault("ScaleComponents", TRUE)
    
    faImage <- session$getImageByType("fa")
    maskImage <- session$getImageByType("mask")
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            output(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    point <- round(c(x,y,z))
    throughPlaneAxis <- which(!is.na(point))
    if (length(throughPlaneAxis) != 1)
        output(OL$Error, "Exactly one of X, Y or Z must be specified")
    inPlaneAxes <- setdiff(1:3, throughPlaneAxis)
    
    point[inPlaneAxes] <- 1
    if (pointType == "fsl")
        point <- transformFslVoxelToRVoxel(point)
    else if (pointType == "mm")
        point <- transformWorldToRVoxel(point, image$getMetadata(), useOrigin=TRUE)
    point[inPlaneAxes] <- NA
    
    createSliceGraphic(faImage, point[1], point[2], point[3], device="internal", windowLimits=windowLimits)
    
    nDirections <- ifelse(source=="tensor", 1, session$nFibres())
    for (i in seq_len(nDirections))
    {
        if (source == "bedpost")
        {
            dyadsImage <- newMriImageFromFile(file.path(session$getBedpostDirectory(), paste("dyads",i,sep="")))
            thresholdImage <- session$getImageByType("avf", i)
        }
        else
        {
            dyadsImage <- newMriImageFromFile(file.path(session$getPreBedpostDirectory(), "dti_V1"))
            thresholdImage <- faImage
        }
        
        fullData <- extractDataFromMriImage(dyadsImage, throughPlaneAxis, point[throughPlaneAxis])
        data <- fullData[,,inPlaneAxes]
        thresholdData <- extractDataFromMriImage(thresholdImage, throughPlaneAxis, point[throughPlaneAxis])
        
        if (scaleComponents)
        {
            data[,,1] <- data[,,1] * thresholdData * 2
            data[,,2] <- data[,,2] * thresholdData * 2
        }

        maskData <- extractDataFromMriImage(maskImage, throughPlaneAxis, point[throughPlaneAxis])
        maskData <- maskData * (thresholdData >= thresholdLevel)

        dims <- faImage$getDimensions()[inPlaneAxes]
        locs <- which(maskData > 0, arr.ind=TRUE)
        d1 <- locs[,1]
        d2 <- locs[,2]
        maskedData <- cbind(data[insertColumnAt(3,locs,1)], data[insertColumnAt(3,locs,2)])
        maskedFullData <- cbind(fullData[insertColumnAt(3,locs,1)], fullData[insertColumnAt(3,locs,2)], fullData[insertColumnAt(3,locs,3)])
        col <- rgb(abs(maskedFullData[,1]), abs(maskedFullData[,2]), abs(maskedFullData[,3]))
        
        segments((d1-1)/(dims[1]-1)-maskedData[,1]/(2*dims[1]), (d2-1)/(dims[2]-1)-maskedData[,2]/(2*dims[2]), (d1-1)/(dims[1]-1)+maskedData[,1]/(2*dims[1]), (d2-1)/(dims[2]-1)+maskedData[,2]/(2*dims[2]), lwd=3, col=col)
    }
    
    ans <- output(OL$Question, "Copy figure to a high-resolution \"png\" file? [yn]")
    if (tolower(ans) == "y")
    {
        outputFileName <- paste(basename(session$getBaseDirectory()), "_", c("x","y","z")[throughPlaneAxis], point[throughPlaneAxis], "_", source, sep="")
        dev.print(png, filename=ensureFileSuffix(outputFileName,"png"), width=16*dims[1], height=16*dims[2], bg="black")
    }
    
    invisible(NULL)
}
