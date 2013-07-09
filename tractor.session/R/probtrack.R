runProbtrackWithSession <- function (session, x = NULL, y = NULL, z = NULL, mode = c("simple","seedmask"), seedMask = NULL, waypointMasks = NULL, requireFile = !force, requireParticlesDir = FALSE, requireImage = FALSE, nSamples = 5000, verbose = FALSE, force = FALSE, expectExists = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (getBedpostNumberOfFibresForSession(session) == 0)
        report(OL$Error, "The \"bedpost\" program has not yet been run for this session")
    
    mode <- match.arg(mode)
    
    if (mode == "seedmask" && !is(seedMask, "MriImage"))
        report(OL$Error, "Seed point mask must be specified in this mode")
    if (!is.null(waypointMasks) && (!is.list(waypointMasks) || !is(waypointMasks[[1]],"MriImage")))
        report(OL$Error, "Waypoint masks must be specified as a list of MriImage objects")
    
    if (mode == "simple")
    {
        if (is.null(x))
            report(OL$Error, "Seed point(s) must be specified in this mode")
        else if (is.matrix(x))
        {
            if (ncol(x) == 3)
                originalSeed <- x
            else if (nrow(x) == 3)
                originalSeed <- t(x)
            requireImage <- FALSE
        }
        else if (is.numeric(x))
            originalSeed <- resolveVector(len=3, x, y, z)
        else
            report(OL$Error, "Seed point(s) must be specified as a vector or matrix")
        
        # Switch to the FSL indexing convention (from 0)
        seed <- originalSeed - 1
    }
    
    bedpostDir <- session$getDirectory("bedpost")
    probtrackDir <- session$getDirectory("probtrack", createIfMissing=TRUE)
    
    previousWorkingDir <- getwd()
    workingDir <- file.path(tempdir(), paste("probtrackx",Sys.getpid(),sep="_"))
    if (!file.exists(workingDir))
        dir.create(workingDir)
    on.exit(setwd(previousWorkingDir))
    setwd(workingDir)

    if (requireFile && mode == "simple")
        outputStem <- file.path(probtrackDir, "IMAGE")
    else
        outputStem <- threadSafeTempFile()
    
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
            seedFile <- threadSafeTempFile()
            if (is.matrix(seed))
                write.table(seed, seedFile, row.names=FALSE, col.names=FALSE)
            else if (is.numeric(seed))
                execute("echo", paste("'", implode(seed,sep=" "), "' >", seedFile, sep=""))
            
            # FSL 4.1.5 changed the way the --dir and -o flags were used
            if (!is.null(fslVersion) && fslVersion >= 40105)
                paramString <- paste("--opd --dir=", dirname(outputStem), " --mode=simple -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", basename(outputStem), ifelse(verbose, " -V 2", ""), sep="")
            else
                paramString <- paste("--opd --dir=", outputStem, " --mode=simple -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " -o ", outputStem, ifelse(verbose, " -V 2", ""), sep="")
            execute("probtrackx", paramString, errorOnFail=TRUE, silent=TRUE)
        
            unlink(seedFile)
        }
            
        result <- list(session=session, seeds=promote(originalSeed,byrow=TRUE), nSamples=nSamples)
        if (requireImage)
            result <- c(result, list(image=readImageFile(outputFile)))
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
        
        seedFile <- threadSafeTempFile()
        writeImageFile(seedMask, seedFile)
        seedPoints <- which(seedMask$getData() > 0, arr.ind=TRUE)
        
        report(OL$Info, "There are ", nrow(seedPoints), " seed points within the mask")
        
        outputDir <- file.path(workingDir, "seedmask")
        outputFile <- file.path(outputDir, "fdt_paths")
        
        if (is.null(waypointMasks))
            waypointString <- NULL
        else
        {
            waypointFiles <- threadSafeTempFile(rep("waypoint",length(waypointMasks)))
            for (i in seq_along(waypointMasks))
                writeImageFile(waypointMasks[[i]], waypointFiles[i])
            waypointListFile <- threadSafeTempFile()
            writeLines(waypointFiles, waypointListFile)
            waypointString <- paste(" --waypoints=", waypointListFile, sep="")
        }
        
        paramString <- paste("--opd --mode=seedmask -x ", seedFile, " --forcedir -s ", bedpostDir, "/merged -m ", bedpostDir, "/nodif_brain_mask -l -c 0.2 -S 2000 --steplength=0.5 -P ", nSamples, " --dir=", outputDir, waypointString, sep="")
        execute("probtrackx", paramString, errorOnFail=TRUE, silent=TRUE)
        
        nRetainedSamples <- as.numeric(readLines(file.path(outputDir, "waytotal")))
        report(OL$Info, nRetainedSamples, " streamlines were retained, of ", nSamples*nrow(seedPoints), " generated")
        
        result <- list(session=session, seeds=seedPoints, nSamples=nRetainedSamples)
        if (requireImage)
            result <- c(result, list(image=readImageFile(outputFile)))
        
        for (i in seq_along(waypointMasks))
            removeImageFilesWithName(waypointFiles[i])
        removeImageFilesWithName(seedFile)
        unlink(outputDir, recursive=TRUE)
    }
    
    invisible (result)
}

retrieveProbtrackStreamline <- function (probtrackResult, i)
{
    m <- as.matrix(read.table(paste(probtrackResult$particlesDir, "/particle", i-1, sep="")))
    
    # Convert back to the R indexing convention
    invisible (m+1)
}

particleFileSizesForResult <- function (probtrackResult)
{
    info <- file.info(list.files(probtrackResult$particlesDir, pattern="particle*", full.names=TRUE))
    return (info$size)
}
