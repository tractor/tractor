runProbtrackWithSession <- function (session, x = NULL, y = NULL, z = NULL, mode = c("simple","seedmask","waypoints"), seedMask = NULL, waypointMasks = NULL, requireFile = !force, requireParticlesDir = FALSE, requireImage = FALSE, nSamples = 5000, verbose = FALSE, force = FALSE, expectExists = FALSE)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    if (!session$isPreprocessed())
        output(OL$Error, "The specified session has not yet been preprocessed with BEDPOST")
    
    mode <- match.arg(mode)
    
    if (mode %in% c("seedmask","waypoints") && !isMriImage(seedMask))
        output(OL$Error, "Seed point mask must be specified in this mode")
    if (mode == "waypoints" && !is.list(waypointMasks) && !isMriImage(waypointMasks[[1]]))
        output(OL$Error, "Waypoint masks must be specified in this mode")
    
    if (mode == "simple")
    {
        if (is.null(x))
            output(OL$Error, "Seed point(s) must be specified in this mode")
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
            output(OL$Error, "Seed point(s) must be specified as a vector or matrix")
        
        seed <- transformRVoxelToFslVoxel(originalSeed)
    }
    
    bedpostDir <- session$getBedpostDirectory()
    probtrackDir <- session$getProbtrackDirectory()
    
    previousWorkingDir <- getwd()
    workingDir <- file.path(tempdir(), "probtrackx")
    if (!file.exists(workingDir))
        dir.create(workingDir)
    setwd(workingDir)
    
    if (requireFile && mode == "simple")
        outputStem <- file.path(probtrackDir, "IMAGE")
    else
        outputStem <- tempfile()
    
    if (mode == "simple")
    {
        outputFile <- paste(outputStem, implode(seed,sep="_"), sep="_")
        
        if (!requireParticlesDir && !force && imageFileExists(outputFile))
            output(OL$Verbose, "Output for this seed point already exists")
        else
        {
            if (expectExists)
                output(OL$Warning, "Output for this seed point was expected to exist, but it does not")
            
            # The 'requireParticlesDir' option implies the 'verbose' option
            if (requireParticlesDir)
                verbose <- TRUE
            
            if (verbose && file.exists("particles"))
                unlink("particles", recursive=TRUE)
        
            output(OL$Info, "Performing single seed tractography with ", nSamples, " samples")

            # Create a temporary file containing the seed point coordinates
            seedFile <- tempfile()
            if (is.vector(seed))
                system(paste("echo '", implode(seed,sep=" "), "' >", seedFile, sep=""))
            else if (is.matrix(seed))
                write.table(seed, seedFile, row.names=FALSE, col.names=FALSE)

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
        output(OL$Info, "Performing seed mask tractography with ", nSamples, " samples per seed...")
        
        seedFile <- tempfile()
        writeMriImageToFile(seedMask, seedFile)
        seedCount <- sum(seedMask$getData() > 0)
        seedCentre <- round(apply(which(seedMask$getData() > 0, arr.ind=TRUE), 2, median))
        
        outputDir <- file.path(workingDir, "seedmask")
        outputFile <- file.path(outputDir, "fdt_paths")
        
        paramString <- paste("--opd --mode=seedmask -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", basename(outputStem), " --dir=", outputDir, " 2>&1", sep="")
        execute("probtrackx", paramString, errorOnFail=TRUE)
        
        result <- list(session=session, seed=as.vector(seedCentre), nSamples=nSamples*seedCount)
        if (requireImage)
            result <- c(result, list(image=newMriImageFromFile(outputFile)))
        
        removeImageFilesWithName(seedFile)
        unlink(outputDir, recursive=TRUE)
    }
    else if (mode == "waypoints")
    {
        output(OL$Info, "Performing tractography with waypoints using ", nSamples, " samples per seed...")
        
        seedFile <- tempfile()
        writeMriImageToFile(seedMask, seedFile)
        seedCentre <- round(apply(which(seedMask$getData() > 0, arr.ind=TRUE), 2, median))
        
        outputDir <- file.path(workingDir, "waypoints")
        outputFile <- file.path(outputDir, "fdt_paths")
        
        waypointFiles <- tempfile(rep("waypoint",length(waypointMasks)))
        for (i in seq_along(waypointMasks))
            writeMriImageToFile(waypointMasks[[i]], waypointFiles[i])
        waypointListFile <- tempfile()
        writeLines(waypointFiles, waypointListFile)
        
        paramString <- paste("--opd --mode=waypoints -x ", seedFile, " --mask2=", waypointListFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", basename(outputStem), " --dir=", outputDir, " 2>&1", sep="")
        execute("probtrackx", paramString, errorOnFail=TRUE)
        
        nRetainedSamples <- as.numeric(readLines(file.path(outputDir, "waytotal")))
        result <- list(session=session, seed=as.vector(seedCentre), nSamples=nRetainedSamples)
        if (requireImage)
            result <- c(result, list(image=newMriImageFromFile(outputFile)))
        
        for (i in seq_along(waypointMasks))
            removeImageFilesWithName(waypointFiles[i])
        removeImageFilesWithName(seedFile)
        #unlink(outputDir, recursive=TRUE)
    }

    setwd(previousWorkingDir)
    invisible (result)
}

runProbtrackForNeighbourhood <- function (session, x, y = NULL, z = NULL, width = 7, mask = FALSE, weights = NULL, weightThreshold = 1e-3, nSamples = 5000, ...)
{
    if (isNeighbourhoodInfo(x))
        neighbourhoodInfo <- x
    else
    {
        centre <- resolveVector(len=3, x, y, z)
        neighbourhoodInfo <- createNeighbourhoodInfo(width, centre=centre)
    }
    
    if (mask)
    {
        metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("t2"))
        data <- generateImageDataForShape("block", metadata$getDimensions(), centre=centre, width=width)
        seedMask <- newMriImageWithData(data, metadata)
        result <- runProbtrackWithSession(session, seedMask=seedMask, mode="seedmask", nSamples=nSamples, ...)
    }
    else if (!is.null(weights))
    {
        if (length(weights) != ncol(neighbourhoodInfo$vectors))
            output(OL$Error, "Number of weights must match the number of points in the seed neighbourhood")
        
        validSeeds <- which(weights >= weightThreshold)
        nValidSeeds <- length(validSeeds)
        output(OL$Info, nValidSeeds, " seed point(s) have weights above the threshold of ", weightThreshold)
        
        if (nValidSeeds > 0)
        {
            metadata <- newMriImageMetadataFromFile(session$getImageFileNameByType("t2"))
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
