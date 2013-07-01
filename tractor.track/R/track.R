trackWithImages <- function (x, y = NULL, z = NULL, maskName, avfNames, thetaNames, phiNames, nSamples = 5000, maxSteps = 2000, stepLength = 0.5, avfThreshold = 0.05, curvatureThreshold = 0.2, useLoopcheck = TRUE, rightwardsVector = NULL, requireImage = TRUE, requireStreamlines = FALSE, terminateOutsideMask = FALSE)
{
    on.exit(.C("clean_up_streamlines", PACKAGE="tractor.track"))
    
    if (!requireImage && !requireStreamlines)
        report(OL$Error, "Neither a visitation map nor a streamline collection was requested")
    if (!is.null(rightwardsVector) && (!is.numeric(rightwardsVector) || length(rightwardsVector) != 3))
    {
        flag(OL$Warning, "Rightwards vector specified is not a numeric 3-vector - ignoring it")
        rightwardsVector <- NULL
    }
    
    if (is(x, "MriImage"))
    {
        if (x$getDimensionality() != 3)
            report(OL$Error, "Seed image should be three-dimensional")
        seeds <- which(x$getData() > 0, arr.ind=TRUE)
    }
    else if (is.matrix(x))
    {
        if (ncol(x) != 3)
            report(OL$Error, "Seed matrix should have three columns")
        seeds <- x
    }
    else
        seeds <- matrix(resolveVector(len=3,x,y,z), nrow=1)
    
    storage.mode(seeds) <- "double"
    nSeeds <- nrow(seeds)
    
    metadata <- readImageFile(maskName, metadataOnly=TRUE)
    dims <- metadata$getDimensions()
    
    lengths <- sapply(list(avfNames,thetaNames,phiNames), length)
    nCompartments <- lengths[1]
    if (!all(lengths == nCompartments))
        report(OL$Error, "AVF, theta and phi image names must be given for every anisotropic compartment")
    
    report(OL$Info, "Running ", nCompartments, "-compartment tractography with ", nSeeds, " seed(s) and ", nSamples, " streamline(s) per seed")
    result <- .Call("track_with_seeds", seeds, as.integer(nSeeds), 1L, maskName, list(avf=avfNames,theta=thetaNames,phi=phiNames), as.integer(nCompartments), as.integer(nSamples), as.integer(maxSteps), as.double(stepLength), as.double(avfThreshold), as.double(curvatureThreshold), as.logical(useLoopcheck), rightwardsVector, as.logical(requireImage), as.logical(requireStreamlines), as.logical(terminateOutsideMask), PACKAGE="tractor.track")
    
    returnValue <- list(seeds=seeds, nSamples=nSamples*nSeeds)
    if (requireImage)
    {
        dim(result[[1]]) <- dims
        returnValue$image <- newMriImageWithData(result[[1]], metadata)
    }
    if (requireStreamlines)
    {
        streamlineMetadata <- newStreamlineTractMetadataFromImageMetadata(metadata, FALSE, "vox")
        returnValue$streamlines <- StreamlineCollectionTract$new(points=result[[2]], startIndices=result[[3]], seedIndices=result[[4]], metadata=streamlineMetadata)
    }
    
    invisible (returnValue)
}

trackWithSession <- function (session, x, y = NULL, z = NULL, maskName = NULL, ...)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    nCompartments <- getBedpostNumberOfFibresForSession(session)
    if (nCompartments == 0)
        report(OL$Error, "The \"bedpost\" program has not yet been run for this session")
    
	if (is.null(maskName))
		maskName <- session$getImageFileNameByType("mask", "diffusion")
	
    avfNames <- session$getImageFileNameByType("avfsamples", "bedpost", index=1:nCompartments)
    thetaNames <- session$getImageFileNameByType("thetasamples", "bedpost", index=1:nCompartments)
    phiNames <- session$getImageFileNameByType("phisamples", "bedpost", index=1:nCompartments)
    
    returnValue <- trackWithImages(x, y, z, maskName, avfNames, thetaNames, phiNames, ...)
    returnValue$session <- session
    invisible (returnValue)
}

