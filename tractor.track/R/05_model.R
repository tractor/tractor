DiffusionModel <- setRefClass("DiffusionModel", fields=list(pointer="externalptr",type="character"), methods=list(
    getPointer = function () { return (pointer) },
    
    getType = function () { return (type) },
    
    track = function (seeds, count, mask, basename, curvatureThreshold = 0.2, useLoopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, rightwardsVector = NULL, requireMap = TRUE, requireStreamlines = FALSE, requireProfile = FALSE, terminateOutsideMask = FALSE, mustLeaveMask = FALSE, jitter = FALSE)
    {
        if (is.character(mask) && length(mask) == 1)
            maskPath <- identifyImageFileNames(mask)$fileStem
        else if (is(mask, "MriImage"))
        {
            maskPath <- threadSafeTempFile("mask")
            writeImageFile(mask, maskPath)
        }
        else
            report(OL$Error, "Mask should be specified as a file name or MriImage object")
        
        mapPath <- streamlinePath <- profilePath <- NULL
        if (requireMap)
            mapPath <- basename
        if (requireStreamlines)
            streamlinePath <- ensureFileSuffix(basename, "trk")
        if (requireProfile)
            profilePath <- ensureFileSuffix(basename, "txt")
        
        .Call("track", pointer, promote(seeds,byrow=TRUE), as.integer(count), maskPath, rightwardsVector, as.integer(maxSteps), as.double(stepLength), as.double(curvatureThreshold), isTRUE(useLoopcheck), isTRUE(terminateOutsideMask), isTRUE(mustLeaveMask), mapPath, streamlinePath, profilePath, 0L, PACKAGE="tractor.track")
    }
))

getBedpostNumberOfFibres <- function (bedpostDir)
{
    i <- 1
    while (imageFileExists(file.path(bedpostDir, es("mean_f#{i}samples"))))
        i <- i + 1
    
    return (i-1)
}

bedpostDiffusionModel <- function (bedpostDir, avfThreshold = 0.05)
{
    if (length(bedpostDir) != 1)
        report(OL$Error, "BEDPOST directory should be specified as a single string")
    if (!file.exists(bedpostDir) || !file.info(bedpostDir)$isdir)
        report(OL$Error, "The specified BEDPOST directory does not exist, or is a file")
    
    nFibres <- getBedpostNumberOfFibres(bedpostDir)
    if (nFibres < 1)
        report(OL$Error, "BEDPOST files do not seem to be present in directory #{bedpostDir}")
    
    files <- list(avf=file.path(bedpostDir, paste0("merged_f",1:nFibres,"samples")),
                  theta=file.path(bedpostDir, paste0("merged_th",1:nFibres,"samples")),
                  phi=file.path(bedpostDir, paste0("merged_ph",1:nFibres,"samples")))
    
    if (!all(imageFileExists(unlist(files))))
        report(OL$Error, "Some BEDPOST files are missing from directory #{bedpostDir}")
    
    pointer <- .Call("createBedpostModel", files, as.double(avfThreshold), PACKAGE="tractor.track")
    
    return (DiffusionModel$new(pointer=pointer, type="bedpost"))
}
