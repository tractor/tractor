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

Registration <- setRefClass("Registration", contains="SerialisableObject", fields=list(source="Registrand", target="Registrand", method="character", n="integer", transforms="list"), methods=list(
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
        initFields(source=source, target=target, method=method, n=count, transforms=xfms, ...)
    },
    
    getMethod = function () { return (method) },
    
    getSource = function () { return (source) },
    
    getTarget = function () { return (target) },
    
    getTransforms = function (indices = 1:n, reverse = FALSE, preferAffine = FALSE, half = FALSE, errorIfMissing = TRUE)
    {
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
        types <- lapply(transforms, function(x) x$getTypes())
        return (table(unlist(types)))
    },
    
    nTransforms = function () { return (n) },
    
    reverse = function () { return (reverseRegistration(.self)) },
    
    setTransforms = function (objects, type, indices = NULL)
    {
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

createRegistration <- function (source, target, method = "fixed", ...)
{
    registration <- Registration$new(source, target, method)
    xfm <- buildAffine(source=source, target=target, ...)
    registration$setTransforms(rep(list(xfm), registration$nTransforms()))
    return (registration)
}

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

registerImages <- function (source, target, registration = NULL, sourceMask = NULL, targetMask = NULL, method = getOption("tractorRegistrationMethod"), types = "affine", affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    method <- match.arg(method, c("niftyreg","fsl"))
    types <- match.arg(types, TransformTypes, several.ok=TRUE)
    
    if (!is.null(registration))
    {
        assert(is(registration,"Registration"), "Registration argument is not of the appropriate class")
        if (missing(source))
            source <- registration$getSource()
        if (missing(target))
            target <- registration$getTarget()
    }
    else
        registration <- Registration$new(source, target, method)
    
    if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(registration, sourceMask=sourceMask, targetMask=targetMask, types=types, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    else if (method == "fsl")
    {
        if (any(types %~% "nonlinear"))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        result <- registerImagesWithFlirt(registration, sourceMask=sourceMask, targetMask=targetMask, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    }
    
    return (result)
}
