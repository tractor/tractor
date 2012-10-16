defaultInfoPanel <- function (point, data, imageNames)
{
    plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", main=paste("Location: (",implode(point,","),")",sep=""))
    nImages <- length(imageNames)
    yLocs <- c(0.9 - 0:(nImages-1) * 0.1, 0)
    labels <- c("Press Esc to exit", paste(imageNames, ": ", sapply(data,function(x) signif(mean(x),6)), sep=""))
    text(rep(0.5,nImages), yLocs, rev(labels))
}

viewImages <- function (images, colourScales = NULL, infoPanel = defaultInfoPanel, ...)
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
    
    point <- round(dims / 2)
    imageNames <- sapply(images, function(x) basename(x$getSource()))
    
    oldPars <- par(bg="black", col="white", fg="white", col.axis="white", col.lab="white", col.main="white")
    
    repeat
    {
        point[point < 1] <- 1
        point[point > dims] <- dims[point > dims]
        voxelCentre <- (point - 0.5) / dims
        
        starts <- ends <- numeric(0)
        
        # Plot the info panel first so that we have some handle on the coordinate system when we use locator()
        layout(matrix(c(2,3,4,1),nrow=2,byrow=TRUE))
        
        if (is.null(infoPanel))
        {
            oldPars <- par(col.main="white")
            plot(1:3, 1:3, main=paste("Location: (",implode(point,","),")",sep=""))
            par(oldPars)
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
            
            lines(rep(voxelCentre[inPlaneAxes[1]],2),c(0,1),col="red")
            lines(c(0,1),rep(voxelCentre[inPlaneAxes[2]],2),col="red")
        }
        
        nextPoint <- locator(1)
        if (is.null(nextPoint))
            break
        
        # Coordinates are relative to the axial plot at this point
        nextPoint <- unlist(nextPoint)
        if (nextPoint[1] > 1 && nextPoint[2] <= 1)
            next
        else if (nextPoint[1] <= 1 && nextPoint[2] > 1)
            point[2:3] <- round((starts[1:2] + (nextPoint %% 1)*(ends[1:2]-starts[1:2])) * dims[2:3] + 0.5)
        else if (nextPoint[1] > 1 && nextPoint[2] > 1)
            point[c(1,3)] <- round((starts[3:4] + (nextPoint %% 1)*(ends[3:4]-starts[3:4])) * dims[c(1,3)] + 0.5)
        else
            point[1:2] <- round((starts[5:6] + (nextPoint %% 1)*(ends[5:6]-starts[5:6])) * dims[1:2] + 0.5)
    }
    
    par(oldPars)
}
