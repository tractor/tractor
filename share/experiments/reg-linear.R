#@args source image file, target image file, [output file]

library(tractor.reg)

runExperiment <- function ()
{
    method <- getConfigVariable("Method", "niftyreg", validValues=c("niftyreg","flirt"), errorIfInvalid=TRUE)
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","sinc","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    targetMaskFile <- getConfigVariable("TargetMaskFile", NULL, "character")
    degreesOfFreedom <- getConfigVariable("DegreesOfFreedom", 12L, "integer")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    transformName <- getConfigVariable("TransformationName", NULL, "character")
    
    # NiftyReg-only options
    nLevels <- getConfigVariable("Levels", 3L, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 5L, "integer")
    useBlockPercentage <- getConfigVariable("UseBlockPercentage", 50L, "integer")
    
    requireArguments("source image file", "target image file")
    
    if (!estimateOnly && nArguments() < 3)
        report(OL$Error, "An output file name is required (as a third argument)")
    
    if (is.null(transformName) && nArguments() >= 3)
        transformName <- Arguments[3]
    
    if (!is.null(initAffineFile))
    {
        initAffine <- RNiftyReg::readAffine(initAffineFile)
        if (is.null(attr(initAffine,"affineType")))
        {
            report(OL$Warning, "Assuming that stored affine matrix uses the ", ifelse(method=="flirt","FSL","NiftyReg"), " convention")
            attr(initAffine,"affineType") <- ifelse(method=="flirt","fsl","niftyreg")
        }
    }
    else
        initAffine <- NULL
    
    result <- registerImages(Arguments[1], Arguments[2], targetMask=targetMaskFile, method=method, types="affine", affineDof=degreesOfFreedom, estimateOnly=estimateOnly, finalInterpolation=interpolation, cache="ignore", initAffine=initAffine, nLevels=nLevels, maxIterations=maxIterations, useBlockPercentage=useBlockPercentage)
    
    result$transform$serialise(ensureFileSuffix(transformName,"Rdata"))
    
    if (!estimateOnly)
        writeImageFile(result$transformedImage, Arguments[3])
}
