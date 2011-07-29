runProbtrackWithSession <- function (session, x = NULL, y = NULL, z = NULL, mode = c("simple","seedmask"), seedMask = NULL, waypointMasks = NULL, requireFile = !force, requireParticlesDir = FALSE, requireImage = FALSE, nSamples = 5000, verbose = FALSE, force = FALSE, expectExists = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (is.na(getBedpostNumberOfFibresForSession(session)))
        report(OL$Error, "The \"bedpost\" program has not yet been run for this session")
    
    mode <- match.arg(mode)
    
    if (mode == "seedmask" && !is(seedMask, "MriImage"))
        report(OL$Error, "Seed point mask must be specified in this mode")
    if (!is.null(waypointMasks) && (!is.list(waypointMasks) || !isMriImage(waypointMasks[[1]])))
        report(OL$Error, "Waypoint masks must be specified as a list of MriImage objects")
    
    if (mode == "simple")
    {
        if (is.null(x))
            report(OL$Error, "Seed point(s) must be specified in this mode")
        else if (is.vector(x))
            originalSeed <- resolveVector(len=3, x, y, z)
        else if (is.matrix(x))
        {
            if (ncol(x) == 3)
                originalSeed <- x
            else if (nrow(x) == 3)
                originalSeed <- t(x)
            requireImage <- FALSE
        }
        else
            report(OL$Error, "Seed point(s) must be specified as a vector or matrix")
        
        seed <- transformRVoxelToFslVoxel(originalSeed)
    }
    
    bedpostDir <- session$getDirectory("bedpost")
    probtrackDir <- session$getDirectory("probtrack", createIfMissing=TRUE)
    
    previousWorkingDir <- getwd()
    workingDir <- file.path(tempdir(), paste("probtrackx",Sys.getpid(),sep="_"))
    if (!file.exists(workingDir))
        dir.create(workingDir)
    setwd(workingDir)
    
    if (requireFile && mode == "simple")
        outputStem <- file.path(probtrackDir, "IMAGE")
    else
        outputStem <- tempfile()
    
    fslVersion <- getFslVersion()
    
    if (mode == "simple")
    {
        outputFile <- paste(outputStem, implode(seed,sep="_"), sep="_")
        
        if (!requireParticlesDir && !force && imageFileExists(outputFile))
            report(OL$Verbose, "Output for this seed point already exists")
        else
        {
            if (expectExists)
                report(OL$Warning, "Output for this seed point was expected to exist, but it does not")
            
            # The 'requireParticlesDir' option implies the 'verbose' option
            if (requireParticlesDir)
                verbose <- TRUE
            
            if (verbose && file.exists("particles"))
                unlink("particles", recursive=TRUE)
        
            report(OL$Info, "Performing single seed tractography with ", nSamples, " samples")

            # Create a temporary file containing the seed point coordinates
            seedFile <- tempfile()
            if (is.vector(seed))
                execute("echo", paste("'", implode(seed,sep=" "), "' >", seedFile, sep=""))
            else if (is.matrix(seed))
                write.table(seed, seedFile, row.names=FALSE, col.names=FALSE)
            
            # FSL 4.1.5 changed the way the --dir and -o flags were used
            if (!is.null(fslVersion) && fslVersion >= 40105)
                paramString <- paste("--opd --dir=", dirname(outputStem), " --mode=simple -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", basename(outputStem), ifelse(verbose, " -V 2", ""), sep="")
            else
                paramString <- paste("--opd --dir=", outputStem, " --mode=simple -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", outputStem, ifelse(verbose, " -V 2", ""), sep="")
            execute("probtrackx", paramString, errorOnFail=TRUE, silent=TRUE)
        
            unlink(seedFile)
        }
            
        result <- list(session=session, seed=originalSeed, nSamples=nSamples)
        if (requireImage)
            result <- c(result, list(image=newMriImageFromFile(outputFile)))
        if (requireParticlesDir)
            result <- c(result, list(particlesDir=file.path(workingDir,"particles")))
        if (requireFile)
            result <- c(result, list(fileName=outputFile))
        else
            removeImageFilesWithName(outputFile)
    }
    else if (mode == "seedmask")
    {
        report(OL$Info, "Performing seed mask tractography with ", nSamples, " samples per seed")
        
        seedFile <- tempfile()
        writeMriImageToFile(seedMask, seedFile)
        seedPoints <- which(seedMask$getData() > 0, arr.ind=TRUE)
        seedCentre <- round(apply(seedPoints, 2, median))
        
        report(OL$Info, "There are ", nrow(seedPoints), " seed points within the mask")
        
        outputDir <- file.path(workingDir, "seedmask")
        outputFile <- file.path(outputDir, "fdt_paths")
        
        if (is.null(waypointMasks))
            waypointString <- NULL
        else
        {
            waypointFiles <- tempfile(rep("waypoint",length(waypointMasks)))
            for (i in seq_along(waypointMasks))
                writeMriImageToFile(waypointMasks[[i]], waypointFiles[i])
            waypointListFile <- tempfile()
            writeLines(waypointFiles, waypointListFile)
            waypointString <- paste(" --waypoints=", waypointListFile, sep="")
        }
        
        paramString <- paste("--opd --mode=seedmask -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " --dir=", outputDir, waypointString, sep="")
        execute("probtrackx", paramString, errorOnFail=TRUE, silent=TRUE)
        
        nRetainedSamples <- as.numeric(readLines(file.path(outputDir, "waytotal")))
        report(OL$Info, nRetainedSamples, " streamlines were retained, of ", nSamples*nrow(seedPoints), " generated")
        
        result <- list(session=session, seed=as.vector(seedCentre), nSamples=nRetainedSamples)
        if (requireImage)
            result <- c(result, list(image=newMriImageFromFile(outputFile)))
        
        for (i in seq_along(waypointMasks))
            removeImageFilesWithName(waypointFiles[i])
        removeImageFilesWithName(seedFile)
        unlink(outputDir, recursive=TRUE)
    }

    setwd(previousWorkingDir)
    invisible (result)
}

runProbtrackForNeighbourhood <- function (session, x, y = NULL, z = NULL, width = 7, mask = FALSE, weights = NULL, weightThreshold = 1e-3, nSamples = 5000, ...)
{
    if (is.list(x))
        neighbourhoodInfo <- x
    else
    {
        centre <- resolveVector(len=3, x, y, z)
        neighbourhoodInfo <- createNeighbourhoodInfo(width, centre=centre)
    }
    
    if (mask)
    {
        metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("maskedb0"))
        data <- generateImageDataForShape("block", metadata$getDimensions(), centre=centre, width=width)
        seedMask <- newMriImageWithData(data, metadata)
        result <- runProbtrackWithSession(session, seedMask=seedMask, mode="seedmask", nSamples=nSamples, ...)
    }
    else if (!is.null(weights))
    {
        if (length(weights) != ncol(neighbourhoodInfo$vectors))
            report(OL$Error, "Number of weights must match the number of points in the seed neighbourhood")
        
        validSeeds <- which(weights >= weightThreshold)
        nValidSeeds <- length(validSeeds)
        report(OL$Info, nValidSeeds, " seed point(s) have weights above the threshold of ", weightThreshold)
        
        if (nValidSeeds > 0)
        {
            metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("maskedb0"))
            sequence <- match(sort(weights[validSeeds]), weights)
            seeds <- t(neighbourhoodInfo$vectors[,sequence])
            runProbtrackWithSession(session, seeds, mode="simple", requireFile=TRUE, nSamples=nSamples)
            
            data <- array(0, dim=metadata$getDimensions())
            for (i in 1:nValidSeeds)
            {
                subResult <- runProbtrackWithSession(session, seeds[i,], mode="simple", requireImage=TRUE, nSamples=nSamples)
                data <- data + subResult$image$getData() * weights[sequence[i]]
            }
            
            result <- subResult
            normalisationFactor <- sum(weights[sequence])
            result$image <- newMriImageWithData(data/normalisationFactor, metadata)
        }
        else
            result <- NULL
    }
    else
        result <- runProbtrackWithSession(session, t(neighbourhoodInfo$vectors), mode="simple", nSamples=nSamples, ...)
    
    invisible(result)
}

retrieveProbtrackStreamline <- function (probtrackResult, i)
{
    m <- as.matrix(read.table(paste(probtrackResult$particlesDir, "/particle", i-1, sep="")))
    invisible (transformFslVoxelToRVoxel(m))
}

particleFileSizesForResult <- function (probtrackResult)
{
    info <- file.info(list.files(probtrackResult$particlesDir, pattern="particle*", full.names=TRUE))
    return (info$size)
}
