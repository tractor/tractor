trackWithImages <- function (x, y = NULL, z = NULL, maskName, avfNames, thetaNames, phiNames, nSamples = 5000, maxSteps = 2000, stepLength = 0.5, avfThreshold = 0.05, curvatureThreshold = 0.2, useLoopcheck = TRUE, rightwardsVector = NULL, requireImage = TRUE, requireStreamlineSet = FALSE)
{
    if (!is.null(rightwardsVector) && (!is.numeric(rightwardsVector) || length(rightwardsVector) != 3))
    {
        flag(OL$Warning, "Rightwards vector specified is not a numeric 3-vector - ignoring it")
        rightwardsVector <- NULL
    }
    
    seed <- resolveVector(len=3, x, y, z)
    maxStepsPerSide <- maxSteps / 2
    
    metadata <- newMriImageMetadataFromFile(maskName)
    dims <- metadata$getDimensions()
    
    lengths <- sapply(list(avfNames,thetaNames,phiNames), length)
    nCompartments <- lengths[1]
    if (!all(lengths == nCompartments))
        report(OL$Error, "AVF, theta and phi image names must be given for every anisotropic compartment")
    
    result <- .C("track_fdt", as.integer(seed),
                              as.character(maskName),
                              as.character(avfNames),
                              as.character(thetaNames),
                              as.character(phiNames),
                              as.integer(nCompartments),
                              as.integer(nSamples),
                              as.integer(maxSteps),
                              as.double(stepLength),
                              as.double(avfThreshold),
                              as.double(curvatureThreshold),
                              as.integer(useLoopcheck),
                              as.integer(!is.null(rightwardsVector)),
                              (if (is.null(rightwardsVector)) NA_real_ else as.double(rightwardsVector)),
                              visitation_counts=integer(prod(dims)),
                              left_lengths=(if (requireStreamlineSet) integer(nSamples) else NA_integer_),
                              right_lengths=(if (requireStreamlineSet) integer(nSamples) else NA_integer_),
                              left_particles=(if (requireStreamlineSet) as.double(rep(NA,maxStepsPerSide*3*nSamples)) else NA_real_),
                              right_particles=(if (requireStreamlineSet) as.double(rep(NA,maxStepsPerSide*3*nSamples)) else NA_real_),
                              as.integer(requireImage),
                              as.integer(requireStreamlineSet),
                              NAOK=TRUE, PACKAGE="tractor.native")
    
    returnValue <- list(seed=seed, nSamples=nSamples)
    if (requireImage)
    {
        dim(result$visitation_counts) <- dims
        newMetadata <- newMriImageMetadataFromTemplate(metadata, datatype=getDataTypeByNiftiCode(8))
        returnValue$image <- newMriImageWithData(result$visitation_counts, newMetadata)
    }
    if (requireStreamlineSet)
    {
        dim(result$left_particles) <- c(maxStepsPerSide, 3, nSamples)
        dim(result$right_particles) <- c(maxStepsPerSide, 3, nSamples)
        streamlineMetadata <- newStreamlineTractMetadataFromImageMetadata(metadata, FALSE, "vox")
        returnValue$streamlineSet <- StreamlineSetTract$new(seedPoint=seed, leftLengths=as.integer(result$left_lengths), rightLengths=as.integer(result$right_lengths), leftPoints=result$left_particles[1:max(result$left_lengths),,], rightPoints=result$right_particles[1:max(result$right_lengths),,], metadata=streamlineMetadata)
    }
    
    invisible (returnValue)
}

trackWithSession <- function (session, x, y = NULL, z = NULL, ...)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    nCompartments <- getBedpostNumberOfFibresForSession(session)
    if (is.na(nCompartments))
        report(OL$Error, "The \"bedpost\" program has not yet been run for this session")
    
    maskName <- session$getImageFileNameByType("mask", "diffusion")
    avfNames <- session$getImageFileNameByType("avfsamples", "bedpost", index=1:nCompartments)
    thetaNames <- session$getImageFileNameByType("thetasamples", "bedpost", index=1:nCompartments)
    phiNames <- session$getImageFileNameByType("phisamples", "bedpost", index=1:nCompartments)
    
    returnValue <- trackWithImages(x, y, z, maskName, avfNames, thetaNames, phiNames, ...)
    returnValue$session <- session
    invisible (returnValue)
}
