#' Determine the installed version of FSL
#'
#' This function determines the version of FSL installed on the system, if
#' any, by reading the `etc/fslversion` file within the directory pointed to
#' by the `FSLDIR` environment variable.
#'
#' @return An integer encoding the FSL version as `10000*major + 100*minor +
#'   patch`, or `NULL` if the version cannot be determined (e.g. because FSL
#'   does not appear to be installed).
#' @author Jon Clayden
#' @export
getFslVersion <- function ()
{
    fslHome <- Sys.getenv("FSLDIR")
    if (!is.character(fslHome) || nchar(fslHome) == 0)
        return (NULL)
    
    versionFile <- file.path(fslHome, "etc", "fslversion")
    if (!file.exists(versionFile))
        return (NULL)
    
    version <- as.integer(unlist(strsplit(readLines(versionFile)[1], ".", fixed=TRUE)))
    if (length(version) < 3)
        return (NULL)
    else
        return (sum(version[1:3] * c(10000, 100, 1)))
}

#' @rdname showImagesInFreeview
#' @export
showImagesInFsleyes <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-cm", lookupTable, sep=" ")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-a", round(100*opacity), sep=" ")
    }
    
    execute("fsleyes", imageFileNames, errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}

#' @rdname showImagesInFreeview
#' @export
showImagesInFslview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-l", lookupTable, sep=" ")
    }
    if (!is.null(opacity))
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames <- paste(imageFileNames, "-t", opacity, sep=" ")
    }
    
    if (!is.null(locateExecutable("fslview_deprecated", errorIfMissing=FALSE)))
        execute("fslview_deprecated", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    else
        execute("fslview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}

#' Create an FSL-style acquisition parameter file for a session
#'
#' This function creates the acquisition parameter file (`acqparams.txt`)
#' required by FSL's `topup` and `eddy` tools, describing the phase-encoding
#' direction and effective echo spacing for each b=0 volume in a session's
#' raw diffusion-weighted data. If the reverse phase-encode volumes are not
#' specified explicitly, the function attempts to identify them automatically
#' by clustering the b=0 volumes by pairwise similarity. If the file already
#' exists it is not regenerated.
#'
#' @param session An [MriSession] object, which must already have a raw
#'   diffusion-weighted data image associated with it.
#' @param reversePEVolumes An optional integer vector giving the indices,
#'   among the b=0 volumes, of those acquired with a reversed phase-encoding
#'   direction. If `NULL`, these are estimated automatically.
#' @param echoSeparation An optional numeric vector of effective echo spacing
#'   values, one per b=0 volume, or a single value to use for all of them. If
#'   `NULL`, values are taken from an `echosep.txt` file in the session's
#'   diffusion directory if one exists, or else a nominal value is used.
#' @param writeBZeroes Boolean value: should the b=0 volumes also be
#'   extracted and written to file (as `b0vols`), for use by `topup`?
#' @return The path to the acquisition parameter file, or `NULL` if the
#'   specified reverse phase-encode volumes do not all have b=0.
#' @author Jon Clayden
#' @export
createAcquisitionParameterFileForSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL, writeBZeroes = TRUE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!session$imageExists("rawdata","diffusion"))
        report(OL$Error, "The specified session does not contain a raw data image")
    
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    
    bValues <- session$getDiffusionScheme()$getBValues()
    bZeroVolumes <- which(bValues == min(bValues))
    nBZeroVolumes <- length(bZeroVolumes)
    bZeroData <- NULL
    
    if (writeBZeroes)
    {
        bZeroData <- session$getImageByType("rawdata", "diffusion", volumes=bZeroVolumes)
        writeImageFile(bZeroData, file.path(targetDir,"b0vols"))
    }
    
    phaseFile <- file.path(targetDir, "acqparams.txt")
    
    # Give the user a chance to override our guesswork, e.g. if their phase-encode direction is not A-P
    if (!file.exists(phaseFile))
    {
        if (is.null(reversePEVolumes))
        {
            report(OL$Info, "Reverse phase-encode volumes not specified - attempting to guess")
            if (is.null(bZeroData))
                bZeroData <- session$getImageByType("rawdata", "diffusion", volumes=bZeroVolumes)
            bZeroVolumeData <- lapply(seq_len(nBZeroVolumes), function(i) extractMriImage(bZeroData,4,i))
            
            similarities <- matrix(0, nBZeroVolumes, nBZeroVolumes)
            for (i in 1:nBZeroVolumes)
            {
                for (j in 1:i)
                    similarities[i,j] <- similarities[j,i] <- RNiftyReg::similarity(bZeroVolumeData[[i]], bZeroVolumeData[[j]])
            }
            distances <- outer(diag(similarities), diag(similarities), pmin) - similarities
            classes <- cutree(hclust(as.dist(distances)), 2)
            reversePEVolumes <- which(classes == which.min(table(classes)))
            report(OL$Info, "#{pluralise('Volume',reversePEVolumes)} #{implode(bZeroVolumes[reversePEVolumes],',',' and ',ranges=TRUE)} #{pluralise('is',reversePEVolumes,plural='are')} least similar to the other b=0 volumes")
        }
        else if (!all(reversePEVolumes %in% bZeroVolumes))
        {
            report(OL$Warning, "Not all reverse phase-encode volumes have b=0")
            return (NULL)
        }
        else
            reversePEVolumes <- match(reversePEVolumes, bZeroVolumes)
    
        # Assuming anterior-posterior phase-encoding direction here
        phaseEncoding <- t(c(0,1,0)) %x% matrix(1,nBZeroVolumes,1)
        phaseEncoding[reversePEVolumes,2] <- -1
    
        # Use the specified echo separation if available, otherwise use the file, or failing that just zero
        if (!is.null(echoSeparation))
            echoSeparation <- rep(echoSeparation, length.out=nBZeroVolumes)
        else
        {
            echoSeparationFile <- file.path(session$getDirectory("diffusion"), "echosep.txt")
            if (file.exists(echoSeparationFile))
            {
                echoSeparation <- as.numeric(readLines(echoSeparationFile))[bZeroVolumes]
                invalid <- (is.na(echoSeparation) | echoSeparation == 0)
                if (all(invalid))
                    echoSeparation <- rep(0.01, nBZeroVolumes)
                else if (all(echoSeparation[!invalid] == echoSeparation[!invalid][1]))
                    echoSeparation[invalid] <- echoSeparation[!invalid][1]
                else
                    echoSeparation[invalid] <- 0.01
            }
            else
                echoSeparation <- rep(0.01, nBZeroVolumes)
        }
    
        lines <- apply(cbind(phaseEncoding,echoSeparation), 1, implode, sep=" ")
        writeLines(lines, phaseFile)
    }
    
    return (phaseFile)
}

#' Run FSL's topup tool for a session
#'
#' This function runs FSL's `topup` tool to estimate and correct for
#' susceptibility-induced distortion in a session's diffusion-weighted data,
#' using reversed phase-encode b=0 volumes, via the `topup` TractoR workflow
#' (see [runWorkflow()]). An acquisition parameter file is created first if
#' one does not already exist (see [createAcquisitionParameterFileForSession()]).
#'
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory.
#' @param reversePEVolumes An optional integer vector giving the indices,
#'   among the b=0 volumes, of those acquired with a reversed phase-encoding
#'   direction. Passed to [createAcquisitionParameterFileForSession()].
#' @param echoSeparation An optional numeric vector, or single value, of
#'   effective echo spacing. Passed to
#'   [createAcquisitionParameterFileForSession()].
#' @return This function is called for its side effect.
#' @seealso [runEddyWithSession()], which can make use of `topup`'s output
#'   during eddy current correction.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runTopupWithSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL)
{
    session <- as(session, "MriSession")
    session$getDirectory("fdt", createIfMissing=TRUE)
    createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation)
    report(OL$Info, "Running topup to correct susceptibility induced distortions...")
    runWorkflow("topup", session)
}

#' Run FSL's eddy tool for a session
#'
#' This function runs FSL's `eddy` tool to correct a session's
#' diffusion-weighted data for eddy-current-induced distortion and subject
#' motion, via the `eddy` TractoR workflow (see [runWorkflow()]). An
#' acquisition parameter file and volume-to-b0 index file are created first
#' if necessary (see [createAcquisitionParameterFileForSession()]), and the
#' resulting affine transforms are read back into the session on completion
#' (see [readEddyCorrectTransformsForSession()]).
#'
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory.
#' @param reversePEVolumes An optional integer vector giving the indices,
#'   among the b=0 volumes, of those acquired with a reversed phase-encoding
#'   direction. Passed to [createAcquisitionParameterFileForSession()].
#' @param echoSeparation An optional numeric vector, or single value, of
#'   effective echo spacing. Passed to
#'   [createAcquisitionParameterFileForSession()].
#' @return The resulting [tractor.reg::Registration] object, invisibly.
#' @seealso [runEddyCorrectWithSession()], which uses the older, simpler
#'   `eddy_correct` tool instead.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runEddyWithSession <- function (session, reversePEVolumes = NULL, echoSeparation = NULL)
{
    session <- as(session, "MriSession")
    targetDir <- session$getDirectory("fdt", createIfMissing=TRUE)
    createAcquisitionParameterFileForSession(session, reversePEVolumes, echoSeparation, writeBZeroes=FALSE)
    session$updateDiffusionScheme()
    
    bValues <- session$getDiffusionScheme()$getBValues()
    bZeroVolumes <- which(bValues == min(bValues))
    indices <- sapply(seq_along(bValues), function(i) {
        j <- i - bZeroVolumes
        which.min(replace(j, j<0, Inf))
    })
    
    indexFile <- file.path(targetDir, "index.txt")
    writeLines(implode(indices," "), indexFile)
    
    report(OL$Info, "Running eddy to remove eddy current induced artefacts...")
    runWorkflow("eddy", session)
    
    readEddyCorrectTransformsForSession(session)
}

#' Run FSL's eddy_correct tool for a session
#'
#' This function runs FSL's older, simpler `eddy_correct` tool to correct a
#' session's diffusion-weighted data for eddy-current-induced distortion, by
#' registering each volume to a single reference volume, via the
#' `eddycorrect` TractoR workflow (see [runWorkflow()]). The resulting affine
#' transforms are read back into the session on completion (see
#' [readEddyCorrectTransformsForSession()]).
#'
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory.
#' @param refVolume An integer giving the index of the volume to use as the
#'   registration target.
#' @return The resulting [tractor.reg::Registration] object, invisibly.
#' @seealso [runEddyWithSession()], which uses the more modern `eddy` tool
#'   instead.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runEddyCorrectWithSession <- function (session, refVolume)
{
    report(OL$Info, "Running eddy_correct to remove eddy current induced artefacts...")
    runWorkflow("eddycorrect", session, ReferenceVolume=refVolume)
    readEddyCorrectTransformsForSession(session)
}

#' Read eddy current correction transforms for a session
#'
#' This function reads the per-volume affine transforms produced by FSL's
#' `eddy` or `eddy_correct` tools (see [runEddyWithSession()] and
#' [runEddyCorrectWithSession()]) for a session, and returns them as a
#' [tractor.reg::Registration] object relating the raw diffusion-weighted data
#' to the reference b=0 volume. The result is also serialised to file within
#' the session's diffusion directory.
#'
#' @param session An [MriSession] object.
#' @param index An optional integer vector giving the volume indices to read
#'   transforms for, when reading from an `eddy_correct` log file. If `NULL`,
#'   transforms for all volumes are read. Ignored when reading `eddy`'s own
#'   parameter file.
#' @return The resulting [tractor.reg::Registration] object, invisibly.
#' @author Jon Clayden
#' @export
readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    sourcePath <- session$getImageFileNameByType("rawdata", "diffusion")
    targetPath <- session$getImageFileNameByType("refb0", "diffusion")
    registration <- tractor.reg::createRegistration(sourcePath, targetPath, "fsl")
    
    eddyParamsFile <- file.path(session$getDirectory("fdt"), "data.eddy_parameters")
    eddyCorrectLogFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (file.exists(eddyParamsFile))
    {
        corrections <- as.matrix(read.table(eddyParamsFile))
        affines <- lapply(seq_len(nrow(corrections)), function(i) RNiftyReg::invertAffine(RNiftyReg::buildAffine(translation=corrections[i,1:3], angles=corrections[i,4:6], source=targetPath)))
    }
    else if (file.exists(eddyCorrectLogFile))
    {
        logLines <- readLines(eddyCorrectLogFile)
        logLines <- subset(logLines, logLines %~% "^[0-9\\-\\. ]+$")
        
        connection <- textConnection(logLines)
        matrices <- as.matrix(read.table(connection))
        close(connection)
        
        if (is.null(index))
            index <- seq_len(nrow(matrices) / 4)
        
        affines <- lapply(index, function(i) RNiftyReg:::convertAffine(matrices[(((i-1)*4)+1):(i*4),], sourcePath, targetPath))
    }
    else
        report(OL$Error, "No eddy current correction log was found")
    
    registration$setTransforms(affines, "affine")
    registration$serialise(file.path(session$getDirectory("diffusion"), "coreg_xfm.Rdata"))
    invisible(registration)
}

#' Run FSL's dtifit tool for a session
#'
#' This function runs FSL's `dtifit` tool to fit diffusion tensors to a
#' session's preprocessed diffusion-weighted data, via the `dtifit` TractoR
#' workflow (see [runWorkflow()]).
#'
#' @param session An [MriSession] object.
#' @param weightedLeastSquares Boolean value: should tensors be fitted with
#'   weighted, rather than ordinary, least-squares?
#' @return This function is called for its side effect.
#' @seealso [estimateDiffusionTensors()] and
#'   [createDiffusionTensorImagesForSession()], which provide an alternative,
#'   pure-R tensor fitting pipeline.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runDtifitWithSession <- function (session, weightedLeastSquares = FALSE)
{
    session$getDirectory("fdt", createIfMissing=TRUE)
    session$updateDiffusionScheme()
    runWorkflow("dtifit", session, WeightedLeastSquares=as.integer(weightedLeastSquares))
}

#' Run FSL's bet tool for a session
#'
#' This function runs FSL's `bet` (Brain Extraction Tool) on a session's
#' reference diffusion b=0 volume, via the `bet-diffusion` TractoR workflow
#' (see [runWorkflow()]).
#'
#' @param session An [MriSession] object.
#' @param intensityThreshold A numeric fractional intensity threshold, between
#'   0 and 1, smaller values giving larger brain outline estimates. Passed to
#'   `bet`'s `-f` option.
#' @param verticalGradient A numeric threshold gradient, applied vertically,
#'   in the range -1 to 1. Positive values give larger brain outlines at the
#'   bottom and smaller ones at the top of the head. Passed to `bet`'s `-g`
#'   option.
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runBetWithSession <- function (session, intensityThreshold = 0.5, verticalGradient = 0)
{
    runWorkflow("bet-diffusion", session, IntensityThreshold=intensityThreshold, VerticalGradient=verticalGradient)
}

#' Run FSL's bedpostx tool for a session
#'
#' This function runs FSL's `bedpostx` tool to fit a ball-and-sticks
#' diffusion model to a session's preprocessed diffusion-weighted data, via
#' the `bedpostx` TractoR workflow (see [runWorkflow()]). Any existing
#' BEDPOSTX output for the session is removed first. A multi-shell model is
#' used automatically if the session's diffusion scheme has more than one
#' nonzero b-value.
#'
#' @param session An [MriSession] object, or a string giving the path to a
#'   session directory.
#' @param nFibres An integer giving the maximum number of fibre orientations
#'   to model per voxel.
#' @return This function is called for its side effect.
#' @seealso [getBedpostNumberOfFibresForSession()], which counts the number
#'   of fibre orientation maps produced by a previous run.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
runBedpostWithSession <- function (session, nFibres = 3)
{
    session <- as(session, "MriSession")
    session$unlinkDirectory("bedpost")
    modelSpecification <- ifelse(session$getDiffusionScheme()$nShells() > 1, "\"-model 2\"", "")
    runWorkflow("bedpostx", session, FibresPerVoxel=nFibres, ModelSpec=modelSpecification)
}

#' Count the fibre orientations modelled by bedpostx for a session
#'
#' This function counts the number of fibre orientation (`"avf"`, anisotropic
#' volume fraction) maps produced by a previous run of FSL's `bedpostx` tool
#' for a session (see [runBedpostWithSession()]).
#'
#' @param session An [MriSession] object.
#' @return The number of fibre orientations modelled, which may be zero if
#'   BEDPOSTX has not been run for the session.
#' @author Jon Clayden
#' @export
getBedpostNumberOfFibresForSession <- function (session)
{
    return (getImageCountForSession(session, "avf", "bedpost"))
}
