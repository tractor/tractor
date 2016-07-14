#@desc Attempt to mask out the part of an image, typically a high-resolution structural image, which corresponds to the face. This operation is usually performed to help preserve participant anonymity, by preventing 3D reconstruction of the face. It is achieved by linearly registering the image to MNI space, reverse-transforming a face mask back to the original space, and then masking out the face region. Where anonymity is important, users are strongly advised to check the results to ensure that the operation has had the desired effect. If no output file is specified then the input file will be overwritten.
#@args image file, [output file]

library(tractor.reg)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("image file")
    
    image <- readImageFile(Arguments[1])
    
    report(OL$Info, "Obtaining transformation from MNI space")
    space <- guessSpace(image, errorIfOutOfSession=FALSE)
    if (is.null(space))
    {
        result <- registerImages(image, getStandardImage("brain",reorder=FALSE), types="affine", estimateOnly=TRUE, linearOptions=list(symmetric=TRUE))
        transform <- result$transform$invert()
    }
    else
    {
        spacePieces <- ore.split(ore(":",syntax="fixed"), space)
        session <- attachMriSession(spacePieces[1])
        transform <- session$getTransformation("mni", spacePieces[2])
    }
    
    report(OL$Info, "Transforming face mask")
    transformedMask <- transformImage(transform, getStandardImage("face",reorder=FALSE), preferAffine=TRUE, interpolation=0)
    
    # Fill the area "under" the transformed mask
    # 1. Find the corners of the bottom slice in the original mask, and transform them to the target space
    # 2. Find a normal to the plane containing those points
    # 3. For each slice below the highest of those points:
    #    a. Find where the normals from each point crosses the slice
    #    b. Find points within the slice that lie within the quadrilateral containing those points
    # NB: This wouldn't work if reordering had any effect on the MNI images, but they're stored LAS
    report(OL$Info, "Extending below face mask")
    faceMaskLocs <- getStandardImage("face")$getNonzeroIndices()
    ranges <- apply(subset(faceMaskLocs, faceMaskLocs[,3]==1), 2, range)
    corners <- transformPoints(transform, as.matrix(expand.grid(ranges[,1],ranges[,2],1)), preferAffine=TRUE)[c(1,2,4,3),]
    normal <- vectorCrossProduct(corners[2,]-corners[1,], corners[4,]-corners[1,])
    normal <- normal / vectorLength(normal)
    
    grid <- as.matrix(expand.grid(1:dim(image)[1], 1:dim(image)[2]))
    for (i in seq_len(max(round(corners[,3]))))
    {
        report(OL$Verbose, "Processing slice #{i}...")
        sliceCorners <- t(apply(corners, 1, function(x) x[1:2] + (i-x[3]) * normal[1:2] / normal[3]))
        remove <- sapply(1:nrow(grid), function(j) {
            # Ref: http://demonstrations.wolfram.com/AnEfficientTestForAPointToBeInAConvexPolygon/
            cc <- t(apply(sliceCorners, 1, "-", grid[j,]))
            signs <- sign(c(cc[2,1]*cc[1,2]-cc[2,2]*cc[1,1], cc[3,1]*cc[2,2]-cc[3,2]*cc[2,1], cc[4,1]*cc[3,2]-cc[4,2]*cc[3,1], cc[1,1]*cc[4,2]-cc[1,2]*cc[4,1]))
            return (all(signs == signs[1]))
        })
        transformedMask[cbind(grid[remove,],i)] <- 1
    }
    
    report(OL$Info, "Applying to image and writing result")
    image$map(function(x,y) ifelse(y==1,0,x), transformedMask)
    image$writeToFile(Arguments[nArguments()])
}
