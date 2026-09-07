#' Create a brain mask image for a session
#'
#' This function creates a brain (foreground) mask for a session's reference
#' diffusion b=0 volume, and writes it to file along with a masked version of
#' the b=0 volume itself. This provides a pure-R alternative to using an
#' external skull-stripping tool such as FSL's `bet` (see
#' [runBetWithSession()]).
#'
#' @param session An [MriSession] object.
#' @param method A string, either `"kmeans"` or `"fill"`. With `"kmeans"`
#'   (the default), foreground voxels are identified using k-means clustering
#'   on the image intensities, followed by connected-component analysis to
#'   retain only the largest cluster, and morphological closing and dilation
#'   to remove gaps. With `"fill"`, every voxel is treated as foreground.
#' @param nClusters An integer giving the number of clusters to use for
#'   k-means clustering, when `method` is `"kmeans"`.
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
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
        
        maskData <- array(0L, dim=t2Image$getDimensions())
        maskData[kmeansResult$cluster != lowSignalCluster] <- 1L
        
        report(OL$Info, "Finding the largest connected component")
        kernel <- mmand::shapeKernel(width=3, dim=3, type="diamond")
        maskData <- mmand::components(maskData, kernel)
        largestIndex <- which.max(table(maskData))
        maskData <- ifelse(!is.na(maskData) & maskData==largestIndex, 1L, 0L)
        
        report(OL$Info, "Applying morphological operations to remove gaps in the mask")
        kernel <- mmand::shapeKernel(width=5, dim=2, type="diamond")
        maskData <- mmand::closing(maskData, kernel)
        kernel <- mmand::shapeKernel(width=3, dim=2, type="diamond")
        maskData <- mmand::dilate(maskData, kernel)
        
        outsideMask <- (maskData == 0)
        report(OL$Info, round((1-sum(outsideMask)/length(outsideMask))*100,2), "% of voxels are classified as foreground")
        
        t2Image[outsideMask] <- 0L
    }
    else if (method == "fill")
    {
        report(OL$Info, "Treating all voxels as \"foreground\"")
        
        maskData <- array(1L, dim=t2Image$getDimensions())
    }
    
    writeImageFile(t2Image, session$getImageFileNameByType("maskedb0"))
    
    mask <- asMriImage(maskData, t2Image)
    writeImageFile(mask, session$getImageFileNameByType("mask","diffusion"))
}
