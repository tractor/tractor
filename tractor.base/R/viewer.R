defaultInfoPanel <- function (point, data, imageNames)
{
    usingQuartz <- isTRUE(names(dev.cur()) == "quartz")
    quitInstructions <- paste(ifelse(usingQuartz,"Press Esc","Right click"), "to exit", sep=" ")
    
    plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", main=paste("Location: (",implode(point,","),")",sep=""))
    nImages <- length(imageNames)
    yLocs <- c(0.9 - 0:(nImages-1) * 0.1, 0)
    labels <- c(quitInstructions, paste(imageNames, ": ", sapply(data,function(x) signif(mean(x),6)), sep=""))
    text(rep(0.5,nImages), yLocs, rev(labels))
}

viewImages <- function (images, colourScales = NULL, point = NULL, interactive = TRUE, crosshairs = TRUE, orientationLabels = TRUE, infoPanel = defaultInfoPanel, ...)
{
    if (is(images, "MriImage"))
        images <- list(images)
    if (!is.list(images))
        report(OL$Error, "Images must be specified in a list")
    
    nImages <- length(images)
    
    if (is.null(colourScales))
        colourScales <- rep(list(1), nImages)
    else if (!is.list(colourScales))
        colourScales <- as.list(colourScales)
    else if (length(colourScales) != nImages)
        colourScales <- rep(colourScales, length.out=nImages)
    
    nDims <- sapply(images, function(x) x$getDimensionality())
    if (any(nDims < 3 | nDims > 4))
        report(OL$Error, "Only 3D and 4D images may currently be used")
    
    dims <- lapply(images, function(x) x$getDimensions()[1:3])
    if (!equivalent(dims, rep(dims[1],length(dims))))
        report(OL$Error, "Dimensions of the specified images do not match")
    dims <- dims[[1]]
    
    images3D <- lapply(images, function(x) {
        if (x$getDimensionality() == 4)
            newMriImageByExtraction(x, 4, 1)
        else
            x
    })
    
    if (is.null(point))
        point <- round(dims / 2)
    imageNames <- sapply(images, function(x) basename(x$getSource()))
    
    labels <- list(c("P","A","I","S"), c("R","L","I","S"), c("R","L","P","A"))
    
    oldPars <- par(bg="black", col="white", fg="white", col.axis="white", col.lab="white", col.main="white")
    oldOptions <- options(locatorBell=FALSE)
    
    repeat
    {
        point[point < 1] <- 1
        point[point > dims] <- dims[point > dims]
        voxelCentre <- (point - 1) / (dims - 1)
        
        starts <- ends <- numeric(0)
        
        # Plot the info panel first so that we have some handle on the coordinate system when we use locator()
        layout(matrix(c(2,3,4,1),nrow=2,byrow=TRUE))
        
        if (is.null(infoPanel))
        {
            mainPars <- par(bg="black", col="black", fg="black", col.axis="black", col.lab="black", col.main="white")
            plot(1:3, 1:3, main=paste("Location: (",implode(point,","),")",sep=""))
            par(mainPars)
        }
        else
        {
            data <- lapply(images, function(image) {
                if (image$getDimensionality() == 4)
                    image[point[1],point[2],point[3],]
                else
                    image[point[1],point[2],point[3]]
            })
            infoPanel(point, data, imageNames, ...)
        }
        
        for (i in 1:3)
        {
            inPlaneAxes <- setdiff(1:3, i)
            currentPoint <- rep(NA, 3)
            currentPoint[i] <- point[i]
            
            createSliceGraphic(images3D[[1]], currentPoint[1], currentPoint[2], currentPoint[3], device="internal", colourScale=colourScales[[1]])
            if (nImages > 1)
            {
                for (j in 2:nImages)
                    createSliceGraphic(images3D[[j]], currentPoint[1], currentPoint[2], currentPoint[3], device="internal", add=TRUE, colourScale=colourScales[[j]])
            }
            
            region <- par("usr")
            starts <- c(starts, region[c(1,3)])
            ends <- c(ends, region[c(2,4)])
            width <- c(region[2]-region[1], region[4]-region[3])
            
            if (crosshairs)
            {
                halfVoxelWidth <- 0.5 / (dims[inPlaneAxes] - 1)
                lines(rep(voxelCentre[inPlaneAxes[1]],2), c(-halfVoxelWidth[2],1+halfVoxelWidth[2]), col="red")
                lines(c(-halfVoxelWidth[1],1+halfVoxelWidth[1]), rep(voxelCentre[inPlaneAxes[2]],2), col="red")
            }
            
            if (orientationLabels)
                text(c(0.1*width[1]+region[1],0.9*width[1]+region[1],0.5*width[2]+region[3],0.5*width[2]+region[3]), c(0.5*width[1]+region[1],0.5*width[1]+region[1],0.1*width[2]+region[3],0.9*width[2]+region[3]), labels=labels[[i]])
        }
        
        if (!interactive)
            break
        
        nextPoint <- locator(1)
        if (is.null(nextPoint))
            break
        
        # Coordinates are relative to the axial plot at this point
        nextPoint <- unlist(nextPoint)
        if (nextPoint[1] > ends[5] && nextPoint[2] <= ends[6])
            next
        else if (nextPoint[1] <= ends[5] && nextPoint[2] > ends[6])
        {
            adjustedPoint <- (nextPoint-c(starts[5],ends[6])) / (ends[5:6]-starts[5:6]) * (ends[1:2]-starts[1:2]) + starts[1:2]
            point[2:3] <- round(adjustedPoint * (dims[2:3] - 1)) + 1
        }
        else if (nextPoint[1] > ends[5] && nextPoint[2] > ends[6])
        {
            adjustedPoint <- (nextPoint-ends[5:6]) / (ends[5:6]-starts[5:6]) * (ends[3:4]-starts[3:4]) + starts[3:4]
            point[c(1,3)] <- round(adjustedPoint * (dims[c(1,3)] - 1)) + 1
        }
        else
            point[1:2] <- round(nextPoint * (dims[1:2] - 1)) + 1
    }
    
    par(oldPars)
    options(oldOptions)
    
    if (interactive)
        dev.off()
    
    invisible(NULL)
}
