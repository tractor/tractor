createMaskImageForSession <- function (session, method = c("kmeans","fill"))
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    method <- match.arg(method)
    
    targetDir <- session$getDirectory("diffusion")
    t2Image <- session$getImageByType("refb0")
    
    if (method == "kmeans")
    {
        report(OL$Info, "Using k-means clustering to identify \"foreground\" voxels")
        
        kmeansResult <- kmeans(as.vector(t2Image$getData()), 2)
        highSignalCluster <- which.max(kmeansResult$centers)
        clusterSizes <- kmeansResult$size
        
        report(OL$Info, round(clusterSizes[highSignalCluster]/sum(clusterSizes)*100,2), "% of voxels are classified as foreground")
        
        maskData <- array(0L, dim=t2Image$getDimensions())
        maskData[kmeansResult$cluster == highSignalCluster] <- 1L
        t2Image[kmeansResult$cluster != highSignalCluster] <- 0L
    }
    else if (method == "fill")
    {
        report(OL$Info, "Treating all voxels as \"foreground\"")
        
        maskData <- array(1L, dim=t2Image$getDimensions())
    }
    
    writeMriImageToFile(t2Image, session$getImageFileNameByType("maskedb0"))
    
    maskMetadata <- newMriImageMetadataFromTemplate(t2Image$getMetadata(), datatype=getDataTypeByNiftiCode(2))
    mask <- newMriImageWithData(maskData, maskMetadata)
    writeMriImageToFile(mask, session$getImageFileNameByType("mask","diffusion"))
}
