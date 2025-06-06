#@args source image file, target image file, [output file]
#@desc Nonlinearly register a source image to a target image, estimating a transformation between them and optionally producing a transformed output image. NiftyReg is used for this operation. The registration can be initialised from an existing transformation (which will be updated), or from an affine text file or control point image.
#@group Registration

library(tractor.reg)

runExperiment <- function ()
{
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    initAffineType <- getConfigVariable("InitialAffineType", NULL, "character", validValues=c("niftyreg","fsl"))
    initControlFile <- getConfigVariable("InitialControlPointFile", NULL, "character")
    sourceMaskFile <- getConfigVariable("SourceMaskFile", NULL, "character")
    targetMaskFile <- getConfigVariable("TargetMaskFile", NULL, "character")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    transformName <- getConfigVariable("TransformName", NULL, "character")
    
    symmetric <- getConfigVariable("Symmetric", TRUE)
    nLevels <- getConfigVariable("Levels", 3L, "integer")
    maxIterations <- getConfigVariable("MaxIterations", 150L, "integer")
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
    
    init <- registration <- NULL
    source <- identifyImageFileNames(Arguments[1])$fileStem
    target <- identifyImageFileNames(Arguments[2])$fileStem
    
    assert(!is.null(transformName) || nArguments() > 2, "Transformation name must be specified if there is no output file")
    
    if (is.null(transformName))
        transformName <- Arguments[3]
    else if (!is.null(registrationPath(transformName)))
    {
        registration <- readRegistration(transformName, validate=FALSE)
        if (!is(registration, "Registration"))
            report(OL$Warning, "Existing transformation file is not valid")
        else if (!symmetric && is.null(initControlFile) && "nonlinear" %in% names(registration$getTypes()))
        {
            report(OL$Info, "Using control point image stored in transformation for initialisation")
            init <- registration$getTransforms(errorIfMissing=FALSE)
        }
        else if (is.null(initControlFile) && is.null(initAffineFile) && "affine" %in% names(registration$getTypes()))
        {
            report(OL$Info, "Using affine matrix stored in transformation for initialisation")
            init <- registration$getTransforms(preferAffine=TRUE, errorIfMissing=FALSE)
        }
    }
    
    if (is.null(registration))
        registration <- createRegistration(source, target, method)
    if (!is.null(initControlFile) && !symmetric)
        init <- RNiftyReg::readNifti(initControlFile)
    else if (!is.null(initAffineFile))
        init <- RNiftyReg::readAffine(initAffineFile, source, target, type=initAffineType)
    
    types <- "nonlinear"
    if (symmetric)
        types <- c("reverse-nonlinear", types)
    
    report(OL$Info, "Performing registration")
    registration <- registerImages(registration=registration, sourceMask=sourceMaskFile, targetMask=targetMaskFile, method="niftyreg", types=types, estimateOnly=estimateOnly, interpolation=interpolation, init=init, nonlinearOptions=list(nLevels=nLevels,maxIterations=maxIterations,nBins=nBins,bendingEnergyWeight=bendingEnergyWeight,linearEnergyWeight=linearEnergyWeight,jacobianWeight=jacobianWeight,finalSpacing=rep(finalSpacing,3),spacingUnit=spacingUnit))
    
    registration$serialise(transformName)
    if (!estimateOnly)
        writeImageFile(registration$getTransformedImage(), Arguments[3])
    
    invisible(NULL)
}
