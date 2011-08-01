#@args [session directory]
#@desc Check the signs of the diffusion gradient directions associated with a session directory (default "."), flip them as necessary, and optionally update the gradient cache. A least-squares diffusion tensor fit is performed for one slice in each orientation, and the principal directions shown for the user to judge whether their orientations are appropriate.
#@interactive TRUE

library(tractor.session)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    useOrthographic <- getConfigVariable("UseOrthographicView", TRUE)
    faThreshold <- getConfigVariable("AnisotropyThreshold", 0.15)
    lineWidth <- getConfigVariable("LineWidth", 1.5)
    updateCache <- getConfigVariable("UpdateGradientCache", FALSE)
    
    report(OL$Info, "Reading data")
    dataImage <- session$getImageByType("data", "diffusion")
    b0Image <- session$getImageByType("maskedb0", "diffusion")
    maskImage <- session$getImageByType("mask", "diffusion")
    biggestLoc <- sapply(1:3, function (i) which.max(apply(maskImage$getData(),i,sum)))
    
    repeat
    {
        if (useOrthographic)
            layout(matrix(1:4,nrow=2,byrow=TRUE))
        
        scheme <- newSimpleDiffusionSchemeFromSession(session)
        
        for (i in 1:3)
        {
            inPlaneAxes <- setdiff(1:3, i)
            point <- rep(NA, 3)
            point[i] <- biggestLoc[i]
            
            createSliceGraphic(b0Image, point[1], point[2], point[3], device="internal")
            
            currentData <- extractDataFromMriImage(dataImage, i, point[i])
            maskData <- extractDataFromMriImage(maskImage, i, point[i])
            dim(currentData) <- c(prod(dim(currentData))/dim(currentData)[3], dim(currentData)[3])
            currentData <- currentData[maskData>0,]
            fit <- estimateDiffusionTensors(currentData, scheme, method="ls")
            print(sapply(fit$eigenvalues,which.max))
            
            fullEigenvectors <- fit$eigenvectors[,1,fit$fa>=faThreshold]
            eigenvectors <- fullEigenvectors[inPlaneAxes,]
            
            dims <- maskImage$getDimensions()[inPlaneAxes]
            locs <- which(maskData > 0, arr.ind=TRUE)
            d1 <- locs[fit$fa>=faThreshold,1]
            d2 <- locs[fit$fa>=faThreshold,2]
            
            col <- rgb(abs(fullEigenvectors[1,]), abs(fullEigenvectors[2,]), abs(fullEigenvectors[3,]))

            segments((d1-1)/(dims[1]-1)-eigenvectors[1,]/(2*dims[1]), (d2-1)/(dims[2]-1)-eigenvectors[2,]/(2*dims[2]), (d1-1)/(dims[1]-1)+eigenvectors[1,]/(2*dims[1]), (d2-1)/(dims[2]-1)+eigenvectors[2,]/(2*dims[2]), lwd=lineWidth, col=col)
        }
        
        repeat
        {
            done <- tolower(ask("Are the principal directions appropriate? [yn; s to show full images in fslview]"))
            if (done %in% c("y","n"))
                break
            else if (done == "s")
            {
                runDtifitWithSession(session)
                showImagesInFslview(session$getImageFileNameByType("fa","diffusion"), session$getImageFileNameByType("eigenvector","diffusion",index=1))
            }
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
    
    if (updateCache)
        updateGradientCacheFromSession(session, force=TRUE)
    
    invisible(NULL)
}
