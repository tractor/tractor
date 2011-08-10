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
        clusterSizes <- kmeansResult$size
        
        report(OL$Info, round(sum(clusterSizes[otherClusters])/sum(clusterSizes)*100,2), "% of voxels are classified as foreground")
        
        maskData <- array(0L, dim=t2Image$getDimensions())
        maskData[kmeansResult$cluster != lowSignalCluster] <- 1L
        t2Image[kmeansResult$cluster == lowSignalCluster] <- 0L
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
