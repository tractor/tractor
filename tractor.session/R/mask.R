createMaskImageForSession <- function (session, method = c("kmeans","fill"))
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    method <- match.arg(method)
    
    targetDir <- session$getPreBedpostDirectory()
    t2Image <- newMriImageFromFile(file.path(targetDir, "nodif"))
    
    if (method == "kmeans")
    {
        output(OL$Info, "Using k-means clustering to identify \"foreground\" voxels")
        
        kmeansResult <- kmeans(t2Image$getData(), 2)
        highSignalCluster <- which.max(kmeansResult$centers)
        clusterSizes <- kmeansResult$size
        
        output(OL$Info, round(clusterSizes[highSignalCluster]/sum(clusterSizes)*100,2), "% of voxels are classified as foreground")
        
        maskData <- array(0L, dim=t2Image$getDimensions())
        maskData[kmeansResult$cluster == highSignalCluster] <- 1L
        t2Image[kmeansResult$cluster != highSignalCluster] <- 0L
    }
    else if (method == "fill")
    {
        output(OL$Info, "Treating all voxels as \"foreground\"")
        
        maskData <- array(1L, dim=t2Image$getDimensions())
    }
    
    writeMriImageToFile(t2Image, file.path(targetDir,"nodif_brain"))
    
    maskMetadata <- newMriImageMetadataFromTemplate(t2Image$getMetadata(), datatype=getDataTypeByNiftiCode(2))
    mask <- newMriImageWithData(maskData, maskMetadata)
    writeMriImageToFile(mask, session$getImageFileNameByType("mask"))
}
