#' Store DICOM series descriptions for a session
#'
#' This function stores a set of (DICOM) series descriptions associated with
#' a session's diffusion-weighted acquisition. These are used to key the
#' gradient direction cache maintained by [checkGradientCacheForSession()] and
#' [updateGradientCacheFromSession()], allowing gradient tables to be reused
#' between sessions acquired with the same protocol.
#'
#' @param session An [MriSession] object.
#' @param descriptions A character vector of DICOM series descriptions, one
#'   per diffusion-weighted volume.
#' @return This function is called for its side effect.
#' @seealso [checkGradientCacheForSession()] and
#'   [updateGradientCacheFromSession()], which read and write the gradient
#'   direction cache keyed on these descriptions.
#' @author Jon Clayden
#' @export
saveSeriesDescriptionsForSession <- function (session, descriptions)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    descriptionString <- implode(gsub("\\W","",descriptions,perl=TRUE), ",")
    writeLines(descriptionString, file.path(session$getDirectory("diffusion"),"descriptions.txt"))
}

#' Check the gradient direction cache for a session
#'
#' This function checks the user's local gradient direction cache (stored
#' under `~/.tractor/gradient-cache`) for a gradient direction set matching a
#' session's diffusion-weighted series descriptions, as previously stored by
#' [saveSeriesDescriptionsForSession()]. This allows a gradient table
#' established for a particular protocol to be looked up and reused for
#' subsequent sessions using the same protocol, without requiring the user to
#' resupply it.
#'
#' @param session An [MriSession] object, which must already have series
#'   descriptions associated with it (see
#'   [saveSeriesDescriptionsForSession()]).
#' @return A numeric matrix with one row per gradient direction and the
#'   b-value in the final column, or `NULL` if no cached entry matches the
#'   session's series descriptions, or none is cached at all.
#' @seealso [updateGradientCacheFromSession()], which adds new entries to the
#'   cache.
#' @author Jon Clayden
#' @export
checkGradientCacheForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    descriptionsFile <- file.path(session$getDirectory("diffusion"), "descriptions.txt")
    if (!file.exists(descriptionsFile))
        return (NULL)
    seriesDescriptions <- readLines(descriptionsFile, 1)
    
    cacheDirectory <- file.path(Sys.getenv("HOME"), ".tractor", "gradient-cache")
    cacheIndexFile <- file.path(cacheDirectory, "index.txt")
    if (!file.exists(cacheIndexFile))
        return (NULL)
    
    cacheIndex <- read.table(cacheIndexFile, col.names=c("descriptions","number"))
    cacheEntry <- subset(cacheIndex, cacheIndex$descriptions==seriesDescriptions)
    
    if (nrow(cacheEntry) != 1)
        return (NULL)
    
    gradientSet <- as.matrix(read.table(file.path(cacheDirectory, paste("set",cacheEntry$number,".txt",sep=""))))
    return (gradientSet)
}

#' Update the gradient direction cache from a session
#'
#' This function adds the (unrotated) gradient directions and b-values
#' associated with a session to the user's local gradient direction cache
#' (stored under `~/.tractor/gradient-cache`), keyed by the session's
#' diffusion-weighted series descriptions (see
#' [saveSeriesDescriptionsForSession()]). This allows the same gradient table
#' to be reused for other sessions acquired with the same protocol; see
#' [checkGradientCacheForSession()].
#'
#' @param session An [MriSession] object, which must already have a diffusion
#'   scheme and series descriptions associated with it.
#' @param force Boolean value: if `TRUE`, replace any existing cache entry for
#'   the session's series descriptions. Otherwise the cache is left unchanged
#'   if a matching entry already exists.
#' @return `TRUE` if the cache was updated, and `FALSE` otherwise (because
#'   the session has no diffusion scheme or series descriptions, or a matching
#'   cache entry already existed and `force` was `FALSE`).
#' @seealso [checkGradientCacheForSession()], which looks up cached entries.
#' @author Jon Clayden
#' @export
updateGradientCacheFromSession <- function (session, force = FALSE)
{
    if (is.null(session$getDiffusionScheme()))
        return (FALSE)
    
    descriptionsFile <- file.path(session$getDirectory("diffusion"), "descriptions.txt")
    if (!file.exists(descriptionsFile))
        return (FALSE)
    seriesDescriptions <- readLines(descriptionsFile, 1)
    
    cacheDirectory <- file.path(Sys.getenv("HOME"), ".tractor", "gradient-cache")
    if (!file.exists(cacheDirectory))
        dir.create(cacheDirectory, recursive=TRUE)
    
    cacheIndexFile <- file.path(cacheDirectory, "index.txt")
    if (!file.exists(cacheIndexFile))
    {
        cacheIndex <- NULL
        number <- 1
    }
    else
    {
        cacheIndex <- read.table(cacheIndexFile, col.names=c("descriptions","number"))
        cacheEntry <- subset(cacheIndex, cacheIndex$descriptions==seriesDescriptions)
        if (nrow(cacheEntry) == 0)
            number <- max(cacheIndex$number) + 1
        else if (!force)
            return (FALSE)
        else
        {
            cacheIndex <- subset(cacheIndex, cacheIndex$descriptions!=seriesDescriptions)
            number <- cacheEntry$number
        }
    }
    
    scheme <- session$getDiffusionScheme(unrotated=TRUE)
    gradientSet <- cbind(scheme$getGradientDirections(), scheme$getBValues())
    write.table(gradientSet, file.path(cacheDirectory,paste("set",number,".txt",sep="")), row.names=FALSE, col.names=FALSE)
    
    cacheIndex <- rbind(cacheIndex, data.frame(descriptions=seriesDescriptions,number=number))
    write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

#' Flip gradient direction components for a session
#'
#' This function negates one or more components of a session's diffusion
#' gradient directions, and writes the updated scheme back to file. This is
#' typically needed to correct for a sign convention mismatch between the
#' scanner and the tractography software being used.
#'
#' @param session An [MriSession] object, which must already have a diffusion
#'   scheme associated with it.
#' @param axes An integer vector giving the gradient direction component(s)
#'   to flip: some subset of 1 (x), 2 (y) and 3 (z).
#' @param unrotated Boolean value: should the unrotated (as opposed to
#'   eddy-current-corrected) gradient directions be flipped?
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @export
flipGradientVectorsForSession <- function (session, axes, unrotated = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    scheme <- session$getDiffusionScheme(unrotated=unrotated)
    directions <- scheme$getGradientDirections()
    directions[,axes] <- (-directions[,axes])
    scheme <- asDiffusionScheme(directions, scheme$getBValues())
    session$updateDiffusionScheme(scheme, unrotated=unrotated)
}

#' Rotate gradient directions to match eddy current correction
#'
#' This function rotates a session's (unrotated) diffusion gradient
#' directions by the rotational component of the affine transforms found by
#' eddy current correction (see [readEddyCorrectTransformsForSession()]), and
#' writes both the unrotated and newly-rotated schemes back to file. This
#' compensates for the fact that eddy current correction realigns the
#' diffusion-weighted volumes, which would otherwise leave the nominal
#' gradient directions inconsistent with the corrected data.
#'
#' @param session An [MriSession] object, which must already have an eddy
#'   current correction transformation associated with it (see
#'   [getVolumeTransformationForSession()]).
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @export
rotateGradientVectorsForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    transformSets <- getVolumeTransformationForSession(session,"diffusion")$getTransformSets()
    decompositions <- lapply(transformSets, function(set) RNiftyReg::decomposeAffine(set$getObject("affine")))
    
    unrotatedScheme <- session$getDiffusionScheme(unrotated=TRUE)
    directions <- unrotatedScheme$getGradientDirections()
    directions <- sapply(1:nrow(directions), function(i) decompositions[[i]]$rotationMatrix %*% directions[i,])
    rotatedScheme <- asDiffusionScheme(t(directions), unrotatedScheme$getBValues())
    session$updateDiffusionScheme(unrotatedScheme, unrotated=TRUE)
    session$updateDiffusionScheme(rotatedScheme)
}
