#' Valid transform types
#' @export
TransformTypes <- c("affine", "nonlinear", "reverse-nonlinear")

.checkAndConvertTransform <- function (xfm, name = NULL)
{
    if (is.null(xfm) || inherits(xfm, "niftyregRDS"))
        return (xfm)
    else if (isAffine(xfm, strict=TRUE) || isImage(xfm, FALSE))
        return (saveTransform(xfm))
    else if (is.null(name) || name == "")
        report(OL$Error, "Transform object is invalid")
    else
        report(OL$Error, "Transform object \"#{name}\" is invalid")
}

#' The TransformSet class
#' 
#' This class represents a set of transformations relating to a single 2D or 3D
#' registration. They may encapsulate linear (affine) and/or nonlinear mappings
#' between the corresponding source and target spaces, and are compatible with
#' the `RNiftyReg` package.
#' 
#' @field objects A list of transform objects, which are stored internally in
#'   the format returned by the [RNiftyReg::saveTransform()] function.
#' 
#' @export
TransformSet <- setRefClass("TransformSet", contains="SerialisableObject", fields=list(objects="list"), methods=list(
    initialize = function (objects = list(), ...)
    {
        initObjects <- c(objects, list(...))
        initObjects <- mapply(.checkAndConvertTransform, initObjects, names(initObjects) %||% "")
        initFields(objects=initObjects, ...)
    },
    
    getObject = function (type)
    {
        type <- match.arg(type, TransformTypes)
        return (loadTransform(objects[[type]]))
    },
    
    getTypes = function () { return (intersect(TransformTypes, names(objects))) },
    
    hasType = function (type)
    {
        type <- match.arg(type, TransformTypes)
        return (!is.null(objects[[type]]))
    },
    
    setObject = function (object, type)
    {
        type <- match.arg(type, TransformTypes)
        .self$objects[[type]] <- .checkAndConvertTransform(object, type)
        invisible(.self)
    }
))

# Union class combining object types that are valid for source and target images
# Character class is for a path
setOldClass("niftiHeader")
setClassUnion("Registrand", c("MriImage","niftiHeader","character"))

#' The Registration class
#' 
#' This class represents a complete image registration operation, including the
#' two images being aligned and a set of associated transformations between
#' them, which may be the result of an optimisation. Transforms can be added to
#' the object after it is first created, allowing for an incremental refinement
#' of the registration, for example from linear to nonlinear.
#' 
#' @field source The source (a.k.a. moving, floating) image, a string giving
#'   the path to it, or a [RNifti::niftiHeader()] object encapsulating its
#'   metadata. May have one more dimension than the target image.
#' @field target The target (a.k.a. fixed, reference) image, providing the
#'   output grid and coordinate system for the registration. Can be in any of
#'   the same forms as the source image.
#' @field method A character vector naming the methods used to create the
#'   transforms.
#' @field n An integer giving the number of transforms stored in the object.
#'   If the source and target images have the same dimensionality this will be
#'   1; otherwise it is the size of the highest dimension of the source image.
#' @field transforms A list of [TransformSet] objects containing the transforms
#'   from source to target space (and potentially the reverse). When a
#'   registration object is first initialised these sets will all be empty.
#' @field transformed. An optional transformed (output) image, or \code{NULL}.
#'   This field is not serialised with the object.
#' 
#' @note `MriImage` source and target images should not be reordered, as
#'   usually performed by [tractor.base::readImageFile()], if the resulting
#'   transforms need to be consistent with the original files.
#' @export
Registration <- setRefClass("Registration", contains="SerialisableObject", fields=list(source="Registrand", target="Registrand", method="character", n="integer", transforms="list", transformed.=Optional("MriImage")), methods=list(
    initialize = function (source = niftiHeader(), target = niftiHeader(), method = "", ...)
    {
        # niftiHeader() works for all valid registrands
        # NB: it's the NIfTI dim field that's extracted here, whose first element is the image dimensionality
        sourceDims <- RNifti::niftiHeader(source)$dim
        targetDims <- RNifti::niftiHeader(target)$dim
        dimDifference <- sourceDims[1] - targetDims[1]
        assert(dimDifference %in% 0:1, "Source image should have the same dimensionality as the target, or one higher")
        count <- ifelse(dimDifference == 0, 1L, sourceDims[sourceDims[1]+1])
        xfms <- rep(list(TransformSet$new()), count)
        initFields(source=source, target=target, method=method, n=count, transforms=xfms, transformed.=NULL, ...)
    },
    
    getMethod = function () { return (method) },
    
    getSource = function () { return (source) },
    
    getTarget = function () { return (target) },
    
    getTransformedImage = function () { return (transformed.) },
    
    getTransforms = function (indices = 1:n, reverse = FALSE, preferAffine = FALSE, half = FALSE, errorIfMissing = TRUE)
    {
        "Extract one or more transforms, favouring nonlinear warps by default"
        if (length(indices) > 1)
            return (lapply(indices, .self$getTransforms, reverse=reverse, preferAffine=preferAffine, half=half, errorIfMissing=errorIfMissing))
        
        transformSet <- transforms[[indices]]
        nonlinearType <- ifelse(reverse, "reverse-nonlinear", "nonlinear")
        
        if (transformSet$hasType("affine") && (preferAffine || !transformSet$hasType(nonlinearType)))
            object <- transformSet$getObject("affine")
        else if (transformSet$hasType(nonlinearType))
            object <- transformSet$getObject(nonlinearType)
        else if (errorIfMissing)
            report(OL$Error, "No suitable #{ifelse(reverse,'reverse','forward')} transform is available for index #{indices}")
        else
            return (NULL)
        
        if (reverse && isAffine(object))
            object <- invertAffine(object)
        if (half)
            object <- halfTransform(object)
        
        return (object)
    },
    
    getTransformSets = function (indices = 1:n) { return (transforms[indices]) },
    
    getTypes = function ()
    {
        "Return a named vector of counts of each type of transform stored"
        types <- lapply(transforms, function(x) x$getTypes())
        return (table(unlist(types)))
    },
    
    nTransforms = function () { return (n) },
    
    reverse = function ()
    {
        "Create an inverted registration with source and target images swapped"
        return (reverseRegistration(.self))
    },
    
    setTransformedImage = function (image)
    {
        "Update the transformed source image"
        .self$transformed. <- where(!is.null(image), as(image,"MriImage"))
        invisible(.self)
    },
    
    setTransforms = function (objects, type, indices = NULL)
    {
        "Add or replace some transforms of a given type, starting from the first by default"
        if (!is.null(objects) && !is.list(objects))
            objects <- list(objects)
        if (is.null(indices))
            indices <- seq_along(objects)
        for (i in seq_along(indices))
            transforms[[indices[i]]]$setObject(objects[[i]], type)
        invisible(.self)
    },
    
    summarise = function ()
    {
        "Summarise the registration"
        sourceSummary <- as(source,"MriImage")$summarise()
        targetSummary <- as(target,"MriImage")$summarise()
        
        methodString <- ifelse(length(method)==0L || method=="", "(unknown)", method)
        types <- .self$getTypes()
        if (length(types) == 0)
            typeSummary <- "(none)"
        else
            typeSummary <- implode(sort(paste0(names(types), " (", types, ")")), ", ")
        
        values <- c(sourceSummary$values[match(c("Image dimensions","Voxel dimensions"), sourceSummary$labels)], targetSummary$values[match(c("Image dimensions","Voxel dimensions"), targetSummary$labels)])
        values <- c(methodString, typeSummary, values)
        names(values) <- c("Registration method", "Stored transformations", "Source image dimensions", "Source voxel dimensions", "Target image dimensions", "Target voxel dimensions")
        
        return (values)
    }
))

#' Read a registration from file
#' 
#' Read a complete registration from file, including all associated transforms.
#' This function handles flat file and directory (.xfmb) formats used by all
#' recent versions of TractoR.
#' 
#' @param path A string giving the path to the file or directory.
#' @param validate Boolean value. If `TRUE`, the default, a deserialised
#'   object is checked to make sure it is of class [Registration].
#' @return A registration object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
readRegistration <- function (path, validate = TRUE)
{
    pathStem <- ensureFileSuffix(ore.subst("/$","",path), NULL, strip=c("xfmb","Rdata"))
    dirPath <- ensureFileSuffix(pathStem, "xfmb")
    filePath <- ensureFileSuffix(pathStem, "Rdata")
    
    if (file.exists(filePath))
    {
        convertImage <- function (rawImage)
        {
            if (!is.null(rawImage$source) && rawImage$source != "")
                return (rawImage$source)
            else if (!is.null(rawImage$data))
                return (deserialiseReferenceObject(object=rawImage))
            else
                report(OL$Error, "Old-style serialised transform file #{filePath} cannot be updated")
        }
        
        fields <- deserialiseReferenceObject(filePath, raw=TRUE)
        
        # Dead-end format only used in prerelease TractoR 3.0
        if ("version" %in% names(fields))
            report(OL$Error, "Transformations from prerelease TractoR version 3.0 must be regenerated")
        # File-based format from TractoR 2.x
        else if (all(c("sourceImage","targetImage") %in% names(fields)))
        {
            if (length(fields$controlPointImages) > 0 || length(fields$reverseControlPointImages) > 0)
                flag(OL$Warning, "Nonlinear transforms from TractoR 2.x registrations may not function as expected - please check any results")
            reg <- Registration$new(convertImage(fields$sourceImage), convertImage(fields$targetImage), fields$method)
            reg$setTransforms(fields$affineMatrices, "affine")
            reg$setTransforms(fields$controlPointImages, "nonlinear")
            reg$setTransforms(fields$reverseControlPointImages, "reverse-nonlinear")
            return (reg)
        }
        # Current format (from TractoR 3.5)
        else
        {
            reg <- deserialiseReferenceObject(object=fields)
            assert(!validate || is(reg,"Registration"), "Serialised object is not a Registration object")
            return (reg)
        }
    }
    # Directory-based format (.xfmb) used by TractoR 3.0 to 3.4
    else if (file.exists(dirPath) && file.info(dirPath)$isdir)
    {
        methodFile <- file.path(dirPath, "method.txt")
        method <- ifelse(file.exists(methodFile), readLines(methodFile)[1], "")
        reg <- Registration$new(file.path(dirPath,"source"), file.path(dirPath,"target"), method)
        
        for (i in seq_len(reg$nTransforms()))
        {
            # Order must match TransformTypes static variable above
            paths <- file.path(dirPath, paste0(c("forward","forward","reverse"), i, c(".mat","","")))
            for (j in seq_along(paths))
            {
                object <- NULL
                if (paths[j] %~% ".mat$" && file.exists(paths[j]))
                    object <- readAffine(paths[j], source=reg$getSource(), target=reg$getTarget())
                else if (imageFileExists(paths[j]))
                    object <- RNiftyReg::readNifti(paths[j], source=reg$getSource(), target=reg$getTarget(), internal=TRUE)
                reg$setTransforms(object, TransformTypes[j], i)
            }
        }
        return (reg)
    }
    else
        report(OL$Error, "No registration found at path #{path}")
}

#' Create an unoptimised registration
#' 
#' This function creates a basic registration object representing a simple
#' linear mapping between the specified source and target images. By default
#' this is essentially an identity transform.
#' 
#' @param source The source (a.k.a. moving, floating) image, a string giving
#'   the path to it, or a [RNifti::niftiHeader()] object encapsulating its
#'   metadata. May have one more dimension than the target image.
#' @param target The target (a.k.a. fixed, reference) image, providing the
#'   output grid and coordinate system for the registration. Can be in any of
#'   the same forms as the source image.
#' @param method A string naming the registration method. This is just a label,
#'   and has no functional consequence.
#' @param ... Additional arguments to [RNiftyReg::buildAffine()], allowing
#'   for rotation angles, translations, etc., to be applied.
#' @return A registration object.
#' @seealso [registerImages()], which creates optimised registrations.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
createRegistration <- function (source, target, method = "identity", ...)
{
    registration <- Registration$new(source, target, method)
    xfm <- buildAffine(source=source, target=target, ...)
    registration$setTransforms(rep(list(xfm), registration$nTransforms()))
    return (registration)
}

#' Reverse a registration
#' 
#' This function inverts the sense of an existing registration, swapping the
#' source and target images and inverting the associated transforms.
#' 
#' @param An existing [Registration] object.
#' @return The reversed registration object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
reverseRegistration <- function (registration)
{
    assert(is(registration,"Registration"), "Registration argument is not of the appropriate class")
    
    transformSets <- registration$getTransformSets()
    reversed <- Registration$new(registration$getTarget(), registration$getSource(), registration$getMethod())
    
    affines <- lapply(transformSets, function (set) {
        affine <- set$getObject("affine")
        return (where(!is.null(affine), invertAffine(affine)))
    })
    reversed$setTransforms(affines, "affine")
    
    irreversible <- sapply(transformSets, function(set) xor(set$hasType("nonlinear"), set$hasType("reverse-nonlinear")))
    assert(sum(irreversible) == 0, "#{sum(irreversible)} nonlinear transforms do not have inverse counterparts", level=OL$Warning)
    
    reversed$setTransforms(lapply(transformSets, function(set) set$getObject("reverse-nonlinear")), "nonlinear")
    reversed$setTransforms(lapply(transformSets, function(set) set$getObject("nonlinear")), "reverse-nonlinear")
    
    return (reversed)
}

#' Register two images
#' 
#' This function performs one or more fully optimised image registrations,
#' finding transforms that maximise the alignment between the transformed
#' source image and the target image with respect to a suitable similarity
#' measure.
#' 
#' The work of the registration is done by the `RNiftyReg` package, or by
#' FSL-FLIRT (which must be installed and on the system path). The function
#' serves as a unified interface to both of these backends. Which one is used
#' depends on the value of the `method` argument.
#' 
#' @param source The source (a.k.a. moving, floating) image, or a string giving
#'   the path to it. Image pixel/voxel data is required for full registration,
#'   so a header object or `MriImage` containing only metadata will not work.
#'   May have one more dimension than the target image, if `method` is
#'   `"niftyreg"`.
#' @param target The target (a.k.a. fixed, reference) image, providing the
#'   output grid and coordinate system for the registration.
#' @param registration An existing [Registration] object to update with the
#'   new transforms, or `NULL` to create a new one. If this argument is
#'   specified then the source and target images will be taken from it, and
#'   should not also be specified.
#' @param sourceMask, targetMask Images in the source and target space
#'   weighting or masking key areas for the optimisation.
#' @param method A string naming the backend to use: `"fsl"` or `"niftyreg"`.
#'   The default is determined by the `tractorRegistrationMethod` option,
#'   which in turn considers the `TRACTOR_REG_METHOD` environment variable. If
#'   none of these are set then the default will be `"niftyreg"`.
#' @param types A vector of transform types to calculate. Must be a subset of
#'   [TransformTypes]. The default is affine-only.
#' @param affineDof The number of degrees of freedom for linear transforms (3D
#'   values are used even if the images are 2D). FSL-FLIRT accepts values of 6
#'   (rigid-body), 7 (global rescale), 9 (traditional) and 12 (affine);
#'   NiftyReg accepts only 6 and 12.
#' @param estimateOnly Boolean value. If `TRUE`, the transform will be
#'   estimated but the source image will not be resampled into the target
#'   space; otherwise the transformed source image will be added to the result.
#' @param interpolation An integer indicating the type of interpolation to
#'   apply when resampling. Both backends accept 0 (nearest neighbour), 1
#'   (blinear or trilinear, the default) and 3 (cubic spline); FSL additionally
#'   accepts 2 (sinc).
#' @param ... Additional arguments to method-specific functions. The FSL
#'   backend currently takes no other arguments; for NiftyReg a list named
#'   `linearOptions` can be used to provide additional arguments to
#'   [RNiftyReg::niftyreg.linear()], and one named `nonlinearOptions` for
#'   [RNiftyReg::niftyreg.nonlinear()].
#' @return A registration object. If the `registration` argument was not `NULL`
#'   then this is an updated version of it.
#' @seealso [createRegistration()], which creates unoptimised registrations
#    such as identity transforms and simple rotations.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
registerImages <- function (source, target, registration = NULL, sourceMask = NULL, targetMask = NULL, method = getOption("tractorRegistrationMethod"), types = "affine", affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    method <- match.arg(tolower(method), c("niftyreg","fsl"))
    types <- match.arg(types, TransformTypes, several.ok=TRUE)
    
    if (is.null(registration))
        registration <- Registration$new(source, target, method)
    else
    {
        assert(is(registration,"Registration"), "Registration argument is not of the appropriate class")
        assert(missing(source) && missing(target), "New source and target images cannot be specified for an existing registration")
    }
    
    # These submethods modify their first argument
    if (method == "niftyreg")
        registerImagesWithNiftyreg(registration, sourceMask=sourceMask, targetMask=targetMask, types=types, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    else if (method == "fsl")
    {
        assert(!any(types %~% "nonlinear"), "FSL-FLIRT does not perform nonlinear registration")
        registerImagesWithFlirt(registration, sourceMask=sourceMask, targetMask=targetMask, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    }
    
    return (registration)
}
