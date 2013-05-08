#@args source image file, target image file, output file

library(tractor.reg)

runExperiment <- function ()
{
    method <- getConfigVariable("Method", "niftyreg", validValues=c("niftyreg","flirt"), errorIfInvalid=TRUE)
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","sinc","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    targetMask <- getConfigVariable("TargetMaskFile", NULL, "character")
    degreesOfFreedom <- getConfigVariable("DegreesOfFreedom", 12L, "integer")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    
    # NiftyReg-only options
    nLevels <- getConfigVariable("NLevels", 3L, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 5L, "integer")
    useBlockPercentage <- getConfigVariable("UseBlockPercentage", 50L, "integer")
    
    requireArguments("source image file", "target image file", "output file")
    
    if (!is.null(initAffineFile))
    {
        initAffine <- RNiftyReg::readAffine(initAffineFile)
        if (is.null(attr(initAffine,"affineType")))
        {
            report(OL$Info, "Assuming that stored affine matrix uses the FSL convention")
            attr(initAffine,"affineType") <- "fsl"
        }
    }
    else
        initAffine <- NULL
    
    result <- registerImages(Arguments[1], Arguments[2], targetMask=targetMask, method=method, types="affine", affineDof=degreesOfFreedom, estimateOnly=estimateOnly, finalInterpolation=interpolation, cache="ignore", initAffine=initAffine, nLevels=nLevels, maxIterations=maxIterations, useBlockPercentage=useBlockPercentage)
    
    RNiftyReg::writeAffine(result$transform$getAffineMatrix(), ensureFileSuffix(Arguments[3],"txt"))
    if (!estimateOnly)
        writeImageFile(result$transformedImage, Arguments[3])
}
