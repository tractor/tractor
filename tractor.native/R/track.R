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
        output(OL$Error, "AVF, theta and phi image names must be given for every anisotropic compartment")
    
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
        require("tractor.nt")
        
        dim(result$left_particles) <- c(maxStepsPerSide, 3, nSamples)
        dim(result$right_particles) <- c(maxStepsPerSide, 3, nSamples)
        streamlineMetadata <- .StreamlineTractMetadata(FALSE, "vox", metadata)
        returnValue$streamlineSet <- .StreamlineSetTract(seed, result$left_lengths, result$right_lengths, result$left_particles[1:max(result$left_lengths),,,drop=FALSE], result$right_particles[1:max(result$right_lengths),,,drop=FALSE], streamlineMetadata)
    }
    
    invisible (returnValue)
}

trackWithSession <- function (session, x, y = NULL, z = NULL, ...)
{
    require("tractor.session")
    
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    nCompartments <- session$nFibres()
    bedpostDir <- session$getBedpostDirectory()
    
    maskName <- session$getImageFileNameByType("mask")
    avfNames <- file.path(bedpostDir, paste("merged_f",seq_len(nCompartments),"samples",sep=""))
    thetaNames <- file.path(bedpostDir, paste("merged_th",seq_len(nCompartments),"samples",sep=""))
    phiNames <- file.path(bedpostDir, paste("merged_ph",seq_len(nCompartments),"samples",sep=""))
    
    returnValue <- trackWithImages(x, y, z, maskName, avfNames, thetaNames, phiNames, ...)
    returnValue$session <- session
    invisible (returnValue)
}
