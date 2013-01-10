createMaskImageForSession <- function (session, method = c("kmeans","fill"), nClusters = 2)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    method <- match.arg(method)
    
    t2Image <- session$getImageByType("refb0", "diffusion")
    
    if (method == "kmeans")
    {
        report(OL$Info, "Using k-means clustering to identify \"foreground\" voxels")
        
        kmeansResult <- kmeans(as.vector(t2Image$getData()), nClusters)
        lowSignalCluster <- which.min(kmeansResult$centers)
        otherClusters <- setdiff(1:nClusters, lowSignalCluster)
        
        maskData <- array(0L, dim=t2Image$getDimensions())
        maskData[kmeansResult$cluster != lowSignalCluster] <- 1L
        
        if (require("mmand"))
        {
            report(OL$Info, "Applying morphological operations to remove gaps in the mask")
            kernel <- shapeKernel(width=5, dim=2, type="diamond", brush=TRUE)
            maskData <- closing(maskData, kernel)
            kernel <- shapeKernel(width=3, dim=2, type="diamond", brush=TRUE)
            maskData <- dilate(maskData, kernel)
        }
        
        outsideMask <- maskData == 0
        report(OL$Info, round((1-sum(outsideMask)/length(outsideMask))*100,2), "% of voxels are classified as foreground")
        
        t2Image[outsideMask] <- 0L
    }
    else if (method == "fill")
    {
        report(OL$Info, "Treating all voxels as \"foreground\"")
        
        maskData <- array(1L, dim=t2Image$getDimensions())
    }
    
    writeImageFile(t2Image, session$getImageFileNameByType("maskedb0"))
    
    mask <- newMriImageWithData(maskData, t2Image)
    writeImageFile(mask, session$getImageFileNameByType("mask","diffusion"))
}
