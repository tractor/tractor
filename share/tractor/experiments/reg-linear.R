#@args source image file, target image file, [output file]
#@desc Linearly register a source image to a target image, estimating a transformation between them and optionally producing a transformed output image. NiftyReg (Method:niftyreg) and FSL-FLIRT (Method:fsl) methods are available, although the latter requires FSL to be installed and the "flirt" executable to be on the user's PATH. FSL-FLIRT allows for 12 (affine), 9 (traditional), 7 (global rescale) or 6 (rigid body) degrees of freedom for 3D registration; NiftyReg allows only 12 or 6. The Symmetric, Levels, MaxIterations and BlockPercentage options apply only to NiftyReg. The registration can be initialised from an existing transformation (which will be updated), or from an affine text file.
#@group Registration

library(tractor.reg)

runExperiment <- function ()
{
    method <- getConfigVariable("Method", "niftyreg", validValues=c("niftyreg","fsl"), errorIfInvalid=TRUE)
    interpolation <- getConfigVariable("Interpolation", "trilinear", validValues=c("nearestneighbour","trilinear","sinc","spline"))
    initAffineFile <- getConfigVariable("InitialAffineFile", NULL, "character")
    initAffineType <- getConfigVariable("InitialAffineType", NULL, "character", validValues=c("niftyreg","fsl"))
    sourceMaskFile <- getConfigVariable("SourceMaskFile", NULL, "character")
    targetMaskFile <- getConfigVariable("TargetMaskFile", NULL, "character")
    degreesOfFreedom <- getConfigVariable("DegreesOfFreedom", 12L, "integer")
    estimateOnly <- getConfigVariable("EstimateOnly", FALSE)
    transformName <- getConfigVariable("TransformName", NULL, "character")
    
    # NiftyReg-only options
    symmetric <- getConfigVariable("Symmetric", TRUE)
    nLevels <- getConfigVariable("Levels", 3L, "integer")
    maxIterations <- getConfigVariable("MaxIterations", 5L, "integer")
    useBlockPercentage <- getConfigVariable("BlockPercentage", 50L, "integer")
    
    requireArguments("source image file", "target image file")
    
    if (!estimateOnly && nArguments() < 3)
        report(OL$Error, "An output file name is required (as a third argument)")
    
    init <- registration <- NULL
    source <- identifyImageFileNames(Arguments[1])$fileStem
    target <- identifyImageFileNames(Arguments[2])$fileStem
    
    assert(!is.null(transformName) || nArguments() > 2, "Transformation name must be specified if there is no output file")
    regFile <- registrationFile(transformName)
    
    if (is.null(transformName))
        transformName <- Arguments[3]
    else if (regFile$present())
    {
        registration <- readRegistration(regFile, validate=FALSE)
        if (!is(registration, "Registration"))
            report(OL$Warning, "Existing transformation file is not valid")
        else if (is.null(initAffineFile) && "affine" %in% names(registration$getTypes()))
        {
            report(OL$Info, "Using affine matrix stored in transformation for initialisation")
            init <- registration$getTransforms(preferAffine=TRUE, errorIfMissing=FALSE)
        }
    }
    
    if (is.null(registration))
        registration <- createRegistration(source, target, method)
    if (!is.null(initAffineFile))
        init <- RNiftyReg::readAffine(initAffineFile, source, target, type=initAffineType)
    
    report(OL$Info, "Performing registration")
    registration <- registerImages(registration=registration, sourceMask=sourceMaskFile, targetMask=targetMaskFile, method=method, types="affine", affineDof=degreesOfFreedom, estimateOnly=estimateOnly, interpolation=interpolation, init=init, linearOptions=list(nLevels=nLevels,maxIterations=maxIterations,useBlockPercentage=useBlockPercentage,symmetric=symmetric))
    
    registration$serialise(transformName)
    if (!estimateOnly)
        writeImageFile(registration$getTransformedImage(), Arguments[3])
    
    invisible(NULL)
}
