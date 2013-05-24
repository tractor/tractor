#@args source image file, target image file, [output file]
#@desc Nonlinearly register a source image to a target image, estimating a transformation between them and optionally producing a transformed output image. NiftyReg is used for this operation. The registration can be initialised from an existing transformation (which will be updated), or from an affine text file or control point image.

library(tractor.reg)

runExperiment <- function ()
{
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    initControlFile <- getConfigVariable("InitialControlPointFile", NULL, "character")
    targetMaskFile <- getConfigVariable("TargetMaskFile", NULL, "character")
    degreesOfFreedom <- getConfigVariable("DegreesOfFreedom", 12L, "integer")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    transformName <- getConfigVariable("TransformationName", NULL, "character")
    
    symmetric <- getConfigVariable("Symmetric", FALSE)
    sourceMaskFile <- getConfigVariable("SourceMaskFile", NULL, "character")
    nLevels <- getConfigVariable("Levels", 3L, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 300L, "integer")
    nBins <- getConfigVariable("HistogramBins", 64L, "integer")
    bendingEnergyWeight <- getConfigVariable("BendingEnergyWeight", 0.005)
    jacobianWeight <- getConfigVariable("JacobianWeight", 0)
    inverseConsistencyWeight <- getConfigVariable("InverseConsistencyWeight", 0.01)
    finalSpacing <- getConfigVariable("ControlPointSpacing", 5L, "integer")
    spacingUnit <- getConfigVariable("SpacingUnit", "vox", validValues=c("vox","mm"))
    
    requireArguments("source image file", "target image file")
    
    if (!estimateOnly && nArguments() < 3)
        report(OL$Error, "An output file name is required (as a third argument)")
    if (!is.null(sourceMaskFile) && !symmetric)
    {
        report(OL$Warning, "Source mask file is not used for nonsymmetric registration")
        sourceMaskFile <- NULL
    }
    
    initAffine <- NULL
    initControl <- NULL
    
    if (is.null(transformName))
    {
        if (nArguments() >= 3)
            transformName <- paste(Arguments[3], "xfm", sep="_")
        else
            report(OL$Error, "Transformation name must be specified if there is no output file")
    }
    else if (file.exists(ensureFileSuffix(transformName,"Rdata")))
    {
        transform <- deserialiseReferenceObject(transformName)
        if (!is(transform, "Transformation"))
            report(OL$Warning, "Existing transformation file is not valid")
        else if (!symmetric && is.null(initControlFile) && "nonlinear" %in% transform$getTypes())
        {
            report(OL$Info, "Using control point image stored in transformation for initialisation")
            initControl <- transform$getControlPointImage()
        }
        else if (is.null(initControlFile) && is.null(initAffineFile) && "affine" %in% transform$getTypes())
        {
            report(OL$Info, "Using affine matrix stored in transformation for initialisation")
            initAffine <- transform$getAffineMatrix()
        }
    }
    
    if (!is.null(initControlFile) && !symmetric)
        initControl <- readImageFile(initControlFile)
    else if (!is.null(initAffineFile))
    {
        initAffine <- RNiftyReg::readAffine(initAffineFile)
        if (is.null(attr(initAffine,"affineType")))
        {
            report(OL$Warning, "Assuming that stored affine matrix uses the NiftyReg convention")
            attr(initAffine,"affineType") <- "niftyreg"
        }
    }
    
    types <- "nonlinear"
    if (symmetric)
        types <- c("reverse-nonlinear", types)
    
    report(OL$Info, "Performing registration")
    result <- registerImages(Arguments[1], Arguments[2], targetMask=targetMaskFile, method="niftyreg", types=types, estimateOnly=estimateOnly, finalInterpolation=interpolation, cache="ignore", initAffine=initAffine, nonlinearOptions=list(initControl=initControl,nLevels=nLevels,maxIterations=maxIterations,nBins=nBins,bendingEnergyWeight=bendingEnergyWeight,jacobianWeight=jacobianWeight,inverseConsistencyWeight=inverseConsistencyWeight,finalSpacing=rep(finalSpacing,3),spacingUnit=spacingUnit))
    
    result$transform$serialise(transformName)
    
    if (!estimateOnly)
        writeImageFile(result$transformedImage, Arguments[3])
    
    invisible(NULL)
}
