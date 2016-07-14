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
        result <- registerImages(Arguments[1], getFileNameForStandardImage("brain"), types="affine", estimateOnly=TRUE, linearOptions=list(symmetric=TRUE))
        transform <- result$transform$invert()
    }
    else
    {
        spacePieces <- ore.split(ore(":",syntax="fixed"), space)
        session <- attachMriSession(spacePieces[1])
        transform <- session$getTransformation("mni", spacePieces[2])
    }
    
    report(OL$Info, "Transforming face mask and applying to image")
    transformedMask <- transformImage(transform, getStandardImage("face",reorder=FALSE), preferAffine=TRUE, interpolation = 0)
    image$map(function(x,y) ifelse(y==1,0,x), transformedMask)
    
    report(OL$Info, "Writing result")
    image$writeToFile(Arguments[nArguments()])
}
