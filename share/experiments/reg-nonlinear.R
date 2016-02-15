#@args source image file, target image file, [output file]
#@desc Nonlinearly register a source image to a target image, estimating a transformation between them and optionally producing a transformed output image. NiftyReg is used for this operation. The registration can be initialised from an existing transformation (which will be updated), or from an affine text file or control point image.

library(tractor.reg)

runExperiment <- function ()
{
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    initControlFile <- getConfigVariable("InitialControlPointFile", NULL, "character")
    sourceMaskFile <- getConfigVariable("SourceMaskFile", NULL, "character")
    targetMaskFile <- getConfigVariable("TargetMaskFile", NULL, "character")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    transformName <- getConfigVariable("TransformationName", NULL, "character")
    
    symmetric <- getConfigVariable("Symmetric", TRUE)
    nLevels <- getConfigVariable("Levels", 3L, "integer")
    maxIterations <- getConfigVariable("MaximumIterations", 300L, "integer")
    nBins <- getConfigVariable("HistogramBins", 64L, "integer")
    bendingEnergyWeight <- getConfigVariable("BendingEnergyWeight", 0.001)
    linearEnergyWeight <- getConfigVariable("LinearEnergyWeight", 0.01)
    jacobianWeight <- getConfigVariable("JacobianWeight", 0)
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
    
    init <- NULL
    
    if (is.null(transformName))
    {
        # Create an output transformation name from output image name
        # This file will NOT be used for initialisation, and will simply be overwritten if it exists
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
            init <- transform$getTransformObjects(1:transform$nRegistrations(), errorIfMissing=FALSE)
        }
        else if (is.null(initControlFile) && is.null(initAffineFile) && "affine" %in% transform$getTypes())
        {
            report(OL$Info, "Using affine matrix stored in transformation for initialisation")
            init <- transform$getTransformObjects(1:transform$nRegistrations(), preferAffine=TRUE, errorIfMissing=FALSE)
        }
    }
    
    if (!is.null(initControlFile) && !symmetric)
        init <- readImageFile(initControlFile)
    else if (!is.null(initAffineFile))
        init <- RNiftyReg::readAffine(initAffineFile)
    
    types <- "nonlinear"
    if (symmetric)
        types <- c("reverse-nonlinear", types)
    
    report(OL$Info, "Performing registration")
    result <- registerImages(Arguments[1], Arguments[2], sourceMask=sourceMaskFile, targetMask=targetMaskFile, method="niftyreg", types=types, estimateOnly=estimateOnly, interpolation=interpolation, cache="ignore", init=init, nonlinearOptions=list(nLevels=nLevels,maxIterations=maxIterations,nBins=nBins,bendingEnergyWeight=bendingEnergyWeight,linearEnergyWeight=linearEnergyWeight,jacobianWeight=jacobianWeight,finalSpacing=rep(finalSpacing,3),spacingUnit=spacingUnit))
    
    result$transform$serialise(transformName)
    
    if (!estimateOnly)
        writeImageFile(result$transformedImage, Arguments[3])
    
    invisible(NULL)
}
