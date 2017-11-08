#@desc Visualise major fibre directions in one slice of a diffusion MRI data set. The slice is selected by setting one of X, Y or Z. The source of the direction information can be "tensor" (in which case there is only one major direction per voxel) or "bedpost". The "tensorfit" or "bedpost" script, respectively, must have already been run on the specified session. Directions will not be shown for components whose FA (for Source:tensor) or volume fraction (for Source:bedpost) are below the specified ThresholdLevel. Direction lengths are scaled by the corresponding FA or volume fraction values unless ScaleComponents:false is given.
#@args session directory
#@interactive TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory")
    session <- attachMriSession(Arguments[1])
    
    x <- getConfigVariable("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getConfigVariable("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getConfigVariable("Z", NA, "numeric", errorIfInvalid=TRUE)
    source <- getConfigVariable("Source", "tensor", validValues=c("bedpost","tensor"))
    thresholdLevel <- getConfigVariable("ThresholdLevel", 0.2)
    maskFile <- getConfigVariable("MaskFile", NULL, "character")
    windowLimits <- getConfigVariable("WindowLimits", NULL, "character")
    scaleComponents <- getConfigVariable("ScaleComponents", TRUE)
    scaleFactor <- getConfigVariable("ScaleFactor", 16)
    
    faImage <- session$getImageByType("fa", "diffusion")
    
    if (is.null(maskFile))
        maskImage <- session$getImageByType("mask", "diffusion")
    else
        maskImage <- readImageFile(maskFile)
    
    if (!is.null(windowLimits))
    {
        windowLimits <- splitAndConvertString(windowLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(windowLimits) != 2)
            report(OL$Error, "Window limits must be given as a 2-vector giving the low and high limits")
    }
    
    point <- round(c(x,y,z))
    throughPlaneAxis <- which(!is.na(point))
    if (length(throughPlaneAxis) != 1)
        report(OL$Error, "Exactly one of X, Y or Z must be specified")
    inPlaneAxes <- setdiff(1:3, throughPlaneAxis)
    
    createSliceGraphic(faImage, point[1], point[2], point[3], device="internal", windowLimits=windowLimits)
    
    nDirections <- ifelse(source=="tensor", 1, getBedpostNumberOfFibresForSession(session))
    if (nDirections == 0)
        report(OL$Error, "The \"bedpost\" program has not yet been run for this session")
    
    insertColumnAt <- function (index, x, colData)
    {
        x <- promote(x, byrow=TRUE)
        end <- ncol(x)
    
        if (index == 1)
            result <- cbind(colData, x)
        else if (index == end+1)
            result <- cbind(x, colData)
        else if (index > 1 && index <= end)
            result <- cbind(x[,1:(index-1),drop=FALSE], colData, x[,index:end,drop=FALSE])
        else
            report(OL$Error, "Index (", index, ") is out of bounds")
    
        return (result)
    }
    
    for (i in seq_len(nDirections))
    {
        if (source == "bedpost")
        {
            dyadsImage <- session$getImageByType("dyads", "bedpost", index=i)
            thresholdImage <- session$getImageByType("avf", "bedpost", index=i)
        }
        else
        {
            dyadsImage <- session$getImageByType("eigenvector", "diffusion", index=1)
            thresholdImage <- faImage
        }
        
        fullData <- dyadsImage$getSlice(throughPlaneAxis, point[throughPlaneAxis])
        data <- fullData[,,inPlaneAxes]
        thresholdData <- thresholdImage$getSlice(throughPlaneAxis, point[throughPlaneAxis])
        
        if (scaleComponents)
        {
            data[,,1] <- data[,,1] * thresholdData * 2
            data[,,2] <- data[,,2] * thresholdData * 2
        }

        maskData <- maskImage$getSlice(throughPlaneAxis, point[throughPlaneAxis])
        maskData <- maskData * (thresholdData >= thresholdLevel)

        dims <- faImage$getDimensions()[inPlaneAxes]
        locs <- which(maskData > 0, arr.ind=TRUE)
        d1 <- locs[,1]
        d2 <- locs[,2]
        maskedData <- cbind(data[insertColumnAt(3,locs,1)], data[insertColumnAt(3,locs,2)])
        maskedFullData <- cbind(fullData[insertColumnAt(3,locs,1)], fullData[insertColumnAt(3,locs,2)], fullData[insertColumnAt(3,locs,3)])
        col <- rgb(abs(maskedFullData[,1]), abs(maskedFullData[,2]), abs(maskedFullData[,3]))
        
        segments((d1-1)/(dims[1]-1)-maskedData[,1]/(2*dims[1]), (d2-1)/(dims[2]-1)-maskedData[,2]/(2*dims[2]), (d1-1)/(dims[1]-1)+maskedData[,1]/(2*dims[1]), (d2-1)/(dims[2]-1)+maskedData[,2]/(2*dims[2]), lwd=round(3*scaleFactor/16), col=col)
    }
    
    ans <- ask("Copy figure to a high-resolution \"png\" file? [yn]")
    if (tolower(ans) == "y")
    {
        outputFileName <- paste(basename(session$getDirectory()), "_", c("x","y","z")[throughPlaneAxis], point[throughPlaneAxis], "_", source, sep="")
        dev.print(png, filename=ensureFileSuffix(outputFileName,"png"), width=round(scaleFactor*dims[1]), height=round(scaleFactor*dims[2]), bg="black")
    }
    
    invisible(NULL)
}
