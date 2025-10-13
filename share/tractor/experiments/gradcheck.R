#@args [session directory]
#@desc Check the signs of the diffusion gradient directions associated with a session directory (default "."), flip them as necessary, and optionally update the gradient cache. By default, a least-squares diffusion tensor fit is performed for one slice in each orientation, and the principal directions shown for the user to judge whether their orientations are appropriate.
#@group Diffusion processing
#@interactive TRUE

library(tractor.session)

runExperiment <- function ()
{
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    useInternalViewer <- getConfigVariable("UseInternalViewer", TRUE)
    useOrthographic <- getConfigVariable("UseOrthographicView", TRUE)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.15)
    lineWidth <- getConfigVariable("LineWidth", 1.5)
    updateCache <- getConfigVariable("UpdateGradientCache", FALSE)
    
    usingRawData <- FALSE
    
    report(OL$Info, "Reading data")
    if (session$imageExists("data", "diffusion"))
    {
        dataImage <- session$getImageByType("data", "diffusion")
        scheme <- session$getDiffusionScheme(unrotated=FALSE)
        b0Image <- session$getImageByType("maskedb0", "diffusion")
        maskImage <- session$getImageByType("mask", "diffusion")
    }
    else
    {
        report(OL$Warning, "Processed data unavailable - raw data will be modified instead")
        usingRawData <- TRUE
        dataImage <- session$getImageByType("rawdata", "diffusion")
        scheme <- session$getDiffusionScheme(unrotated=TRUE)
        b0Image <- asMriImage(dataImage[,,,which.min(scheme$getBValues())], dataImage)
        maskImage <- b0Image$copy()$fill(1L)
    }
    biggestLoc <- sapply(1:3, function (i) which.max(apply(maskImage$getData(),i,sum)))
    
    if (useInternalViewer)
    {
        repeat
        {
            if (useOrthographic)
                layout(matrix(1:4,nrow=2,byrow=TRUE))

            for (i in 1:3)
            {
                inPlaneAxes <- setdiff(1:3, i)
                point <- rep(NA, 3)
                point[i] <- biggestLoc[i]

                createSliceGraphic(b0Image, point[1], point[2], point[3], device="internal")

                currentData <- dataImage$getSlice(i, point[i])
                maskData <- maskImage$getSlice(i, point[i])
                dim(currentData) <- c(prod(dim(currentData))/dim(currentData)[3], dim(currentData)[3])
                currentData <- currentData[maskData>0,]
                fit <- estimateDiffusionTensors(currentData, scheme, method="ls")

                fullEigenvectors <- fit$eigenvectors[,1,fit$fa>=faThreshold]
                eigenvectors <- fullEigenvectors[inPlaneAxes,]

                dims <- maskImage$getDimensions()[inPlaneAxes]
                locs <- which(maskData > 0, arr.ind=TRUE)
                d1 <- locs[fit$fa>=faThreshold,1]
                d2 <- locs[fit$fa>=faThreshold,2]
                
                valid <- (apply(is.na(fullEigenvectors),2,sum) == 0)
                col <- rep(NA, ncol(fullEigenvectors))
                col[valid] <- rgb(abs(fullEigenvectors[1,valid]), abs(fullEigenvectors[2,valid]), abs(fullEigenvectors[3,valid]))

                segments((d1-1)/(dims[1]-1)-eigenvectors[1,]/(2*dims[1]), (d2-1)/(dims[2]-1)-eigenvectors[2,]/(2*dims[2]), (d1-1)/(dims[1]-1)+eigenvectors[1,]/(2*dims[1]), (d2-1)/(dims[2]-1)+eigenvectors[2,]/(2*dims[2]), lwd=lineWidth, col=col)
            }

            if (usingRawData)
                done <- ask("Are the principal directions appropriate? [yn]", valid=c("y","n"))
            else
            {
                done <- ask("Are the principal directions appropriate? [yn; s to show full images in fslview]", valid=c("y","n","s"))
                if (done == "s")
                {
                    runDtifitWithSession(session)
                    showImagesInViewer(session$getImageFileNameByType("fa","diffusion"), session$getImageFileNameByType("eigenvector","diffusion",index=1), viewer="fslview")
                    done <- ask("Are the principal directions appropriate? [yn]", valid=c("y","n"))
                }
            }

            if (done == "y")
                break
            else
            {
                ans <- tolower(ask("Flip diffusion gradient vectors along which axes? [xyz; Enter for none]"))
                flipAxes <- which(letters[24:26] %in% splitAndConvertString(ans, ",", fixed=TRUE))

                if (length(flipAxes) > 0)
                    flipGradientVectorsForSession(session, flipAxes, unrotated=usingRawData)
            }
        }
    }
    else if (!usingRawData)
    {
        repeat
        {
            runDtifitWithSession(session)
            
            repeat
            {
                done <- ask("Are the principal directions appropriate? [yn; s to show images in fslview]", valid=c("y","n","s"))
                if (done == "s")
                    showImagesInViewer(session$getImageFileNameByType("fa","diffusion"), session$getImageFileNameByType("eigenvector","diffusion",index=1), viewer="fslview")
                else
                    break
            }

            if (done == "y")
                break
            else
            {
                ans <- tolower(ask("Flip diffusion gradient vectors along which axes? [xyz; Enter for none]"))
                flipAxes <- which(letters[24:26] %in% splitAndConvertString(ans, ",", fixed=TRUE))

                if (length(flipAxes) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
            }
        }
    }
    
    if (updateCache)
        updateGradientCacheFromSession(session, force=TRUE)
    
    invisible(NULL)
}
