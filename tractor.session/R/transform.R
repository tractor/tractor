.resolveSpace <- function (space, session)
{
    spacePieces <- unlist(strsplit(space, ":", fixed=TRUE))
    if (length(spacePieces) == 2)
    {
        if (spacePieces[1] != session$getDirectory())
            report(OL$Error, "The current space does not correspond to the specified session")
        else
            return (spacePieces[2])
    }
    else
        return (spacePieces[1])
}

.constructSpace <- function (space, session)
{
    if (tolower(space) == "mni")
        return ("mni")
    else if (space %~% ore(":",syntax="fixed"))
        return (space)
    else
        return (paste(session$getDirectory(), space, sep=":"))
}

.findTransformation <- function (image, session, newSpace, oldSpace = NULL)
{
    if (is.null(oldSpace))
        oldSpace <- guessSpace(image, session)
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session))
    
    return (transform)
}

#' Guess the space associated with an image
#'
#' This function attempts to work out which named space (such as `"mni"`,
#' or a session-relative space such as `"diffusion"`) an image belongs to, by
#' comparing the directory containing the image to the standard locations
#' associated with a session, or to the location of TractoR's standard-space
#' reference images.
#'
#' @param image An [tractor.base::MriImage] object, or a string giving the
#'   path to an image file. Internal (in-memory only) images are not
#'   associated with any space.
#' @param session An [MriSession] object, or `NULL` to try to infer the
#'   session from the image's path (which will only work if the image lies
#'   within a session directory).
#' @param errorIfOutOfSession Boolean value: if `TRUE`, an error is raised if
#'   `session` is `NULL` and cannot be inferred from the image's path;
#'   otherwise `NULL` is returned in that case.
#' @return A string naming the space, in the form understood by
#'   [transformImageToSpace()] and related functions, or `NULL` if the space
#'   cannot be determined.
#' @author Jon Clayden
#' @export
guessSpace <- function (image, session = NULL, errorIfOutOfSession = TRUE)
{
    if (is.character(image))
        path <- image
    else if (image$isInternal())
        return (NULL)
    else
        path <- image$getSource()
    
    if (dirname(path) == .StandardBrainPath)
        return ("mni")
    else
    {
        if (is.null(session))
        {
            if (path %~% "^(.+)/tractor")
                session <- attachMriSession(ore.lastmatch()[,1])
            else if (errorIfOutOfSession)
                report(OL$Error, "Image does not seem to be within a session directory - the session must be specified")
            else
                return (NULL)
        }
        
        for (space in setdiff(names(.RegistrationTargets),"mni"))
        {
            if (dirname(path) == session$getDirectory(space))
                return (es("#{session$getDirectory()}:#{space}"))
        }
    }
    
    return (NULL)
}

#' Transform data between named spaces associated with a session
#'
#' These functions transform an image, parcellation or set of points between
#' two named spaces (such as `"mni"`, or a session-relative space such as
#' `"diffusion"` or `"structural"`) associated with a session, obtaining or
#' creating the required registration via the session's
#' `getTransformation()` method, and then delegating the actual transform
#' application to [tractor.reg::transformImage()],
#' [tractor.reg::transformParcellation()] or
#' [tractor.reg::transformPoints()] as appropriate.
#'
#' @param image An [tractor.base::MriImage] object, or a string giving the
#'   path to an image file, to transform.
#' @param session An [MriSession] object.
#' @param newSpace A string naming the space to transform into. See
#'   [guessSpace()] for the space naming convention.
#' @param oldSpace A string naming the current space of the data, or `NULL`
#'   to infer it from `image` or `points`, or (for
#'   `transformParcellationToSpace()`) to default to `"structural"`.
#' @param preferAffine Boolean value: if `TRUE`, use an affine transform even
#'   if a nonlinear one is available.
#' @param interpolation An integer indicating the type of interpolation to
#'   apply: 0 (nearest-neighbour), 1 (linear, the default) or 3 (cubic
#'   spline). Passed to [tractor.reg::transformImage()].
#' @param parcellation A list in the form created by
#'   [tractor.reg::readParcellation()], representing a labelled image and
#'   associated metadata.
#' @param threshold The minimum interpolated value for a label to be retained
#'   in the transformed parcellation. Passed to
#'   [tractor.reg::transformParcellation()].
#' @param points A numeric vector specifying a single point to transform, or
#'   a matrix with one point per row.
#' @param pointType A string giving the convention used by `points`: one of
#'   `"fsl"`, `"r"`/`"vox"` (voxel indices) or `"mm"` (world coordinates). If
#'   `NULL`, this is taken from the `"pointType"` attribute of `points`.
#' @param outputVoxel Boolean value: if `TRUE` and `pointType` is `"mm"`,
#'   convert the transformed points back to voxel coordinates in the target
#'   space before returning them.
#' @param nearest Boolean value: should the resulting points be rounded to
#'   the nearest integer? Passed to [tractor.reg::transformPoints()].
#' @return `transformImageToSpace()` returns a transformed
#'   [tractor.base::MriImage] object. `transformParcellationToSpace()`
#'   returns a transformed parcellation, in the same form as `parcellation`.
#'   `transformPointsToSpace()` returns a numeric vector or matrix of
#'   transformed points, with `"space"` and `"pointType"` attributes set to
#'   reflect the new space and point convention.
#' @seealso [guessSpace()], for the space naming convention used by these
#'   functions; [changePointType()], for converting between point
#'   conventions within a single space.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
transformImageToSpace <- function (image, session, newSpace, oldSpace = NULL, preferAffine = FALSE, interpolation = 1)
{
    transform <- .findTransformation(image, session, newSpace, oldSpace)
    newImage <- tractor.reg::transformImage(transform, image, preferAffine=preferAffine, interpolation=interpolation)
    
    return (newImage)
}

#' @rdname transformImageToSpace
#' @export
transformParcellationToSpace <- function (parcellation, session, newSpace, oldSpace = "structural", threshold = 0.5, preferAffine = FALSE)
{
    transform <- .findTransformation(parcellation$image, session, newSpace, oldSpace)
    newParcellation <- tractor.reg::transformParcellation(transform, parcellation, threshold=threshold, preferAffine=preferAffine)
    
    return (newParcellation)
}

#' @rdname transformImageToSpace
#' @export
transformPointsToSpace <- function (points, session, newSpace, oldSpace = NULL, pointType = NULL, outputVoxel = FALSE, preferAffine = FALSE, nearest = FALSE)
{
    if (is.null(pointType))
    {
        pointType <- attr(points, "pointType")
        if (is.null(pointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    pointType <- match.arg(tolower(pointType), c("fsl","r","vox","mm"))
    if (pointType == "fsl")
        points <- points + 1
    
    if (is.null(oldSpace))
        oldSpace <- attr(points, "space")
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session))
    
    if (outputVoxel && pointType == "mm")
    {
        points <- changePointType(points, transform$getSource(), "r", "mm")
        pointType <- "r"
    }
    
    newPoints <- tractor.reg::transformPoints(transform, points, voxel=(pointType!="mm"), preferAffine=preferAffine, nearest=nearest)
    
    attr(newPoints, "space") <- .constructSpace(newSpace, session)
    attr(newPoints, "pointType") <- ifelse(pointType=="mm", "mm", "r")
    
    return (newPoints)
}

#' Convert points between coordinate conventions
#'
#' This function converts a set of points from one coordinate convention to
#' another, within a single image space: between voxel indices (FSL's
#' zero-based convention, or R's one-based convention) and world (millimetre)
#' coordinates.
#'
#' @param points A numeric vector specifying a single point to convert, or a
#'   matrix with one point per row.
#' @param image An [tractor.base::MriImage] object, or [RNifti::niftiHeader()]
#'   object, giving the target space and its voxel-to-world mapping.
#' @param newPointType A string giving the convention to convert to: one of
#'   `"fsl"`, `"r"`/`"vox"` (voxel indices) or `"mm"` (world coordinates).
#' @param oldPointType A string giving the current convention of `points`. If
#'   `NULL`, this is taken from the `"pointType"` attribute of `points`.
#' @return The converted points, with a `"pointType"` attribute set to
#'   reflect `newPointType`.
#' @seealso [transformPointsToSpace()], which additionally transforms points
#'   between different image spaces.
#' @author Jon Clayden
#' @export
changePointType <- function (points, image, newPointType, oldPointType = NULL)
{
    if (is.null(oldPointType))
    {
        oldPointType <- attr(points, "pointType")
        if (is.null(oldPointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    # NB: point types "r" and "vox" are equivalent
    oldPointType <- match.arg(tolower(oldPointType), c("fsl","r","vox","mm"))
    newPointType <- match.arg(tolower(newPointType), c("fsl","r","vox","mm"))
    
    offsets <- list(fsl=1, r=0, vox=0)
    
    if (oldPointType == newPointType)
        newPoints <- points
    else if (oldPointType == "mm" && newPointType != "mm")
        newPoints <- RNifti::worldToVoxel(points, image) - offsets[[newPointType]]
    else if (oldPointType != "mm" && newPointType == "mm")
        newPoints <- RNifti::voxelToWorld(points + offsets[[oldPointType]], image)
    else
        newPoints <- points + offsets[[oldPointType]] - offsets[[newPointType]]
    
    attr(newPoints, "pointType") <- ifelse(newPointType=="vox", "r", newPointType)
    
    return (newPoints)
}

#' Coregister the volumes of a 4D data set within a session
#'
#' This function coregisters each volume of a session's raw 4D data (of the
#' specified `type`, e.g. `"diffusion"` or `"functional"`) to a common
#' reference volume, and writes the resulting registered data set to file.
#' This is typically used for motion correction of functional or diffusion
#' time series that are not otherwise handled by a dedicated tool such as
#' FSL's `eddy` (see [runEddyWithSession()]).
#'
#' @param session An [MriSession] object.
#' @param type A string giving the (session-relative) type of the 4D data,
#'   e.g. `"diffusion"` or `"functional"`.
#' @param reference An integer index into the fourth dimension of the raw
#'   data, giving the volume to register to, or an
#'   [tractor.base::MriImage] to use as the target directly.
#' @param useMask Boolean value: should the session's mask for `type` be used
#'   to weight or restrict the registration?
#' @param nLevels An integer giving the number of resolution levels to use for
#'   the registration.
#' @param method A string, one of `"niftyreg"`, `"fsl"` or `"none"`. With
#'   `"none"`, an identity transform is stored and the raw data is simply
#'   copied to the coregistered data location.
#' @param options A list of additional method-specific linear registration
#'   options.
#' @param ... Additional arguments to [tractor.reg::registerImages()].
#' @return The resulting [tractor.reg::Registration] object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
coregisterDataVolumesForSession <- function (session, type, reference = 1, useMask = FALSE, nLevels = 2, method = c("niftyreg","fsl","none"), options = list(), ...)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    if (is(reference, "MriImage"))
        targetImage <- reference
    else
        targetImage <- session$getImageByType("rawdata", type, volumes=reference)
    
    sourceMetadata <- session$getImageByType("rawdata", type, metadataOnly=TRUE)
    if (sourceMetadata$getDimensionality() != 4)
        report(OL$Error, "The raw data image is not 4-dimensional")
    nVolumes <- sourceMetadata$getDimensions()[4]
    
    if (method == "none")
    {
        report(OL$Info, "Storing identity transforms")
        registration <- createRegistration(sourceMetadata, targetImage)
        
        report(OL$Info, "Mapping data volume")
        target <- session$getImageFileNameByType("data", type)
        session$imageFiles("rawdata",type)$map(target)
    }
    else
    {
        if (useMask)
            maskImage <- session$getImageByType("mask", type)
        else
            maskImage <- NULL
        
        if (method == "niftyreg")
        {
            report(OL$Info, "Coregistering data to reference volume...")
            registration <- tractor.reg::registerImages(session$getImageFileNameByType("rawdata",type), targetImage, targetMask=maskImage, types="affine", method="niftyreg", ..., linearOptions=c(list(nLevels=nLevels,sequentialInit=TRUE),options))
            
            report(OL$Info, "Writing out transformed data")
            writeImageFile(registration$getTransformedImage(), session$getImageFileNameByType("data",type))
        }
        else
        {
            finalArray <- array(NA, dim=sourceMetadata$getDimensions())
            registration <- createRegistration(sourceMetadata, targetImage)
            
            # FLIRT interface can only handle one volume at a time
            report(OL$Info, "Coregistering data to reference volume...")
            for (i in seq_len(nVolumes))
            {
                report(OL$Verbose, "Reading and registering volume ", i)
                volume <- session$getImageByType("rawdata", type, volumes=i)
                currentReg <- tractor.reg::registerImages(volume, targetImage, targetMask=maskImage, types="affine", method=method, ..., linearOptions=c(list(nLevels=nLevels),options))
                finalArray[,,,i] <- currentReg$getTransformedImage()$getData()
                registration$setTransforms(currentReg$getTransforms(), "affine")
            }
        
            report(OL$Info, "Writing out transformed data")
            finalImage <- asMriImage(finalArray, sourceMetadata)
            writeImageFile(finalImage, session$getImageFileNameByType("data",type))
        }
    }
    
    registration$serialise(file.path(session$getDirectory(type), "coreg"))
    return (registration)
}

#' Read the volume-to-volume transformation for a session
#'
#' This function reads the registration relating the raw volumes of a
#' session's 4D data of a particular `type` to their common reference volume,
#' as produced by [coregisterDataVolumesForSession()] or, for diffusion data,
#' by eddy current correction (see [readEddyCorrectTransformsForSession()]).
#'
#' @param session An [MriSession] object.
#' @param type A string giving the (session-relative) type of the 4D data,
#'   e.g. `"diffusion"` or `"functional"`.
#' @return The relevant [tractor.reg::Registration] object.
#' @author Jon Clayden
#' @export
getVolumeTransformationForSession <- function (session, type)
{
    assert(is(session,"MriSession"), "Specified session is not an MriSession object")
    
    directory <- session$getDirectory(type)
    transformDirName <- file.path(directory, "coreg.xfmb")
    transformFileName <- file.path(directory, "coreg_xfm.Rdata")
    
    if (dir.exists(transformDirName) || file.exists(ensureFileSuffix(transformDirName,"Rdata","xfmb")))
        return (tractor.reg::readRegistration(transformDirName))
    else if (file.exists(transformFileName))
        return (tractor.reg::readRegistration(transformFileName))
    else if (type == "diffusion")
        return (readEddyCorrectTransformsForSession(session))
    else
        report(OL$Error, "Transformation file does not exist for #{type} data")
}
