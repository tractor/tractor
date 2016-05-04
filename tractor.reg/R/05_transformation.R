Transformation <- setRefClass("Transformation", contains="SerialisableObject", fields=list(directory="character",method="character",n="integer",inverted.="logical"), methods=list(
    initialize = function (directory = "", source = NULL, target = NULL, ...)
    {
        method <- "niftyreg"
        if (length(directory) == 1 && directory != "")
        {
            directory <- ensureFileSuffix(directory, "xfmb")
            if (!file.exists(directory))
                dir.create(directory, recursive=TRUE)
            
            methodFile <- file.path(directory, "method.txt")
            if (file.exists(methodFile))
                method <- readLines(methodFile)[1]
            
            if (is.character(source))
                symlinkImageFiles(source, file.path(directory,"source"), overwrite=TRUE)
            else if (isImage(source))
                writeNifti(source, file.path(directory,"source.nii.gz"))
            
            if (is.character(target))
                symlinkImageFiles(target, file.path(directory,"target"), overwrite=TRUE)
            else if (isImage(target))
                writeNifti(target, file.path(directory,"target.nii.gz"))
        }
        else
            directory <- ""
        
        initFields(directory=directory, method=method, n=integer(0), inverted.=FALSE)
    },
    
    getDirectory = function () { return (directory) },
    
    getMethod = function () { return (method) },
    
    getSourceImage = function (i = NULL, reverse = FALSE, ...)
    {
        imageName <- ifelse(xor(reverse,inverted.), "target", "source")
        image <- readImageFile(file.path(directory,imageName), reorder=FALSE, ...)
        
        if (imageName == "target" || is.null(i) || .self$nRegistrations() == 1)
            return (image)
        else
            return (suppressWarnings(extractMriImage(image, ndim(image), i)))
    },
    
    getSourceImagePath = function (i = NULL, reverse = FALSE)
    {
        imageName <- ifelse(xor(reverse,inverted.), "target", "source")
        return (file.path(directory, imageName))
    },
    
    getTargetImage = function (i = NULL, reverse = FALSE, ...)
    {
        imageName <- ifelse(xor(reverse,inverted.), "source", "target")
        image <- readImageFile(file.path(directory,imageName), reorder=FALSE, ...)
        
        if (imageName == "target" || is.null(i) || .self$nRegistrations() == 1)
            return (image)
        else
            return (suppressWarnings(extractMriImage(image, ndim(image), i)))
    },
    
    getTargetImagePath = function (i = NULL, reverse = FALSE)
    {
        imageName <- ifelse(xor(reverse,inverted.), "source", "target")
        return (file.path(directory, imageName))
    },
    
    getTransformObjects = function (i = 1, reverse = FALSE, preferAffine = FALSE, half = FALSE, errorIfMissing = TRUE)
    {
        if (length(i) > 1)
            return (lapply(i, .self$getTransformObjects, reverse=reverse, preferAffine=preferAffine, errorIfMissing=errorIfMissing))
        
        object <- NULL
        source <- .self$getSourceImage(i, reverse, metadataOnly=TRUE)
        target <- .self$getTargetImage(i, reverse, metadataOnly=TRUE)
        
        fileStem <- file.path(directory, ifelse(xor(reverse,inverted.),"reverse","forward"))
        if (file.exists(es("#{fileStem}#{i}.mat")) && (preferAffine || !file.exists(es("#{fileStem}#{i}.nii.gz"))))
            object <- readAffine(es("#{fileStem}#{i}.mat"), source=source, target=target)
        else if (file.exists(es("#{fileStem}#{i}.nii.gz")))
            object <- structure(readNifti(es("#{fileStem}#{i}.nii.gz"),internal=TRUE), source=source, target=target)
        else if (errorIfMissing)
            report(OL$Error, "No suitable #{basename(fileStem)} transform is available for index #{i}")
        
        if (half && !is.null(object))
            object <- halfTransform(object)
        
        return (object)
    },
    
    getTypes = function ()
    {
        transformTypeNames <- c("affine", "nonlinear", "reverse-nonlinear")
        regularExpressions <- c("^forward\\d+\\.mat$", "^forward\\d+\\.nii.gz$", "^reverse\\d+\\.nii.gz$")
        fileNames <- list.files(directory)
        availability <- sapply(regularExpressions, function(re) any(fileNames %~% re))
        return (transformTypeNames[availability])
    },
    
    invert = function ()
    {
        .self$inverted. <- !.self$inverted.
        return (.self)
    },
    
    isInverted = function () { return (.self$inverted.) },
    
    move = function (newDirectory)
    {
        newDirectory <- ensureFileSuffix(newDirectory, "xfmb")
        if (.self$directory == newDirectory)
            return (.self)
        
        # Symlinks will need updating
        oldSource <- Sys.readlink(identifyImageFileNames(.self$getSourceImagePath(reverse=inverted.))$imageFile)
        oldTarget <- Sys.readlink(identifyImageFileNames(.self$getTargetImagePath(reverse=inverted.))$imageFile)
        
        if (isTRUE(file.rename(directory, newDirectory)))
        {
            if (!is.na(oldSource) && oldSource != "")
                symlinkImageFiles(file.path(directory,oldSource), file.path(newDirectory,"source"), overwrite=TRUE)
            if (!is.na(oldTarget) && oldTarget != "")
                symlinkImageFiles(file.path(directory,oldTarget), file.path(newDirectory,"target"), overwrite=TRUE)
            .self$directory <- newDirectory
        }
        else
            flag(OL$Warning, "Could not move transformation file to directory #{newDirectory}")
        return (.self)
    },
    
    nRegistrations = function ()
    {
        if (length(.self$n) != 1)
        {
            source <- .self$getSourceImage(metadataOnly=TRUE)
            target <- .self$getTargetImage(metadataOnly=TRUE)
            if (ndim(source) == ndim(target))
                .self$n <- 1L
            else
                .self$n <- dim(source)[ndim(source)]
        }
        return (.self$n)
    },
    
    summarise = function ()
    {
        sourceSummary <- .self$getSourceImage(metadataOnly=TRUE)$summarise()
        targetSummary <- .self$getTargetImage(metadataOnly=TRUE)$summarise()
        
        values <- c(sourceSummary$values[match(c("Image dimensions","Voxel dimensions"), sourceSummary$labels)], targetSummary$values[match(c("Image dimensions","Voxel dimensions"), targetSummary$labels)])
        values <- c(.self$getMethod(), implode(.self$getTypes(),", "), .self$isInverted(), values)
        names(values) <- c("Registration method", "Stored transformations", "Currently inverted", "Source image dimensions", "Source voxel dimensions", "Target image dimensions", "Target voxel dimensions")
        
        return (values)
    },
    
    updateFromObjects = function (affineMatrices = NULL, controlPointImages = NULL, reverseControlPointImages = NULL, method = c("niftyreg","fsl"))
    {
        if (!is.null(affineMatrices))
        {
            lapply(seq_along(affineMatrices), function(i) {
                if (isAffine(affineMatrices[[i]]))
                {
                    writeAffine(affineMatrices[[i]], file.path(directory,es("forward#{i}.mat")))
                    writeAffine(invertAffine(affineMatrices[[i]]), file.path(directory,es("reverse#{i}.mat")))
                }
            })
        }
        
        if (!is.null(controlPointImages))
        {
            lapply(seq_along(controlPointImages), function(i) {
                if (isImage(controlPointImages[[i]]))
                    writeNifti(controlPointImages[[i]], file.path(directory,es("forward#{i}.nii.gz")))
            })
        }
        
        if (!is.null(reverseControlPointImages))
        {
            lapply(seq_along(reverseControlPointImages), function(i) {
                if (isImage(reverseControlPointImages[[i]]))
                    writeNifti(reverseControlPointImages[[i]], file.path(directory,es("reverse#{i}.nii.gz")))
            })
        }
        
        writeLines(method, file.path(directory,"method.txt"))
        .self$method <- match.arg(method)
        return (.self)
    },
    
    updateFromResult = function (result)
    {
        if (!("niftyreg" %in% class(result)))
            report(OL$Error, "The specified result is not a \"niftyreg\" object")
        
        if (length(result$forwardTransforms) > 0 && isAffine(result$forwardTransforms[[1]]))
            .self$updateFromObjects(affineMatrices=result$forwardTransforms, method="niftyreg")
        else if (length(result$forwardTransforms) > 0 && isImage(result$forwardTransforms[[1]]))
            .self$updateFromObjects(controlPointImages=result$forwardTransforms, method="niftyreg")
        if (length(result$reverseTransforms) > 0 && isImage(result$reverseTransforms[[1]]))
            .self$updateFromObjects(reverseControlPointImages=result$reverseTransforms, method="niftyreg")
        
        return (.self)
    }
))

plot.Transformation <- function (x, y = NULL, xLoc = NA, yLoc = NA, zLoc = NA, sourceImage = NULL, index = 1, preferAffine = FALSE, reverse = FALSE, ...)
{
    reorderPoints <- function (points, image)
    {
        xform <- image$getXform()
        orientation <- tractor.base:::xformToOrientation(xform, string=FALSE)
        dimPermutation <- match(1:3, abs(orientation))
        points <- points[,dimPermutation,drop=FALSE]
        
        ordering <- orientation[dimPermutation]
        if (any(ordering < 0))
        {
            dims <- image$getDimensions()[1:3]
            for (i in which(ordering < 0))
                points[,i] <- dims[i] - points[,i] + 1
        }
        
        return (points)
    }
    
    loc <- c(xLoc, yLoc, zLoc)
    throughPlaneAxis <- which(!is.na(loc))
    if (length(throughPlaneAxis) != 1)
        report(OL$Error, "Exactly one element of the location should be specified")
    inPlaneAxes <- setdiff(1:3, throughPlaneAxis)
    
    if (is.null(sourceImage))
    {
        sourceImage <- x$getSourceImage(index, reverse)
        
        if (sourceImage$isEmpty())
        {
            if (sourceImage$isInternal())
                report(OL$Error, "Original image source is unknown - a source image must be provided")
            sourceImage <- readImageFile(sourceImage$getSource())
        }
        else if (!sourceImage$isReordered())
            sourceImage <- reorderMriImage(sourceImage)
    }
    
    # Calculate the deformation field
    deformationField <- RNiftyReg::deformationField(x$getTransformObjects(index,reverse,preferAffine), jacobian=TRUE)
    
    # Find the requested slice of the Jacobian map and deformation field
    jacobian <- reorderMriImage(as(jacobian(deformationField),"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    field <- reorderMriImage(as(deformationField,"MriImage"))$getSlice(throughPlaneAxis, loc[throughPlaneAxis])
    
    # Remove the last row and column from the data, because NiftyReg seems to calculate it wrongly
    # fieldDims <- dim(field)
    # jacobian <- jacobian[1:(fieldDims[1]-1),1:(fieldDims[2]-1)]
    # field <- field[1:(fieldDims[1]-1),1:(fieldDims[2]-1),,]
    
    # Convert the field to voxel positions and find the closest plane (on average) in source space
    fieldDims <- dim(field)
    dim(field) <- c(prod(fieldDims[1:2]), fieldDims[3])
    fieldVoxels <- reorderPoints(worldToVoxel(field,sourceImage$getMetadata()), sourceImage$getMetadata())
    sourceLoc <- rep(NA, 3)
    sourceLoc[throughPlaneAxis] <- round(mean(fieldVoxels[,throughPlaneAxis], na.rm=TRUE))
    fieldVoxels <- fieldVoxels[,inPlaneAxes]
    
    # Use the 2.5% trimmed range of the whole Jacobian map to create a colour scale (centred at 1)
    jacobianTrimmedRange <- quantile(abs(deformationField$jacobian@.Data), c(0.025,0.975), na.rm=TRUE) - 1
    vizLimit <- max(abs(jacobianTrimmedRange))
    colourIndices <- 1 + round(99 * ((abs(jacobian)-1) + vizLimit) / (2*vizLimit))
    colourIndices[colourIndices < 1] <- 1
    colourIndices[colourIndices > 100] <- 100
    colours <- paste(getColourScale(4)$colours, "60", sep="")
    
    # Create the visualisation
    createSliceGraphic(sourceImage, sourceLoc[1], sourceLoc[2], sourceLoc[3])
    width <- sourceImage$getDimensions()[inPlaneAxes] - 1
    points((fieldVoxels[,1]-1)/width[1], (fieldVoxels[,2]-1)/width[2], pch=3, col=colours[colourIndices])
}

# Read either an older .Rdata file or a new .xfmb folder, or create the latter
attachTransformation <- function (path, source = NULL, target = NULL)
{
    pathStem <- ensureFileSuffix(path, NULL, strip=c("xfmb","Rdata"))
    dirPath <- ensureFileSuffix(pathStem, "xfmb")
    filePath <- ensureFileSuffix(pathStem, "Rdata")
    if (file.exists(dirPath) && file.info(dirPath)$isdir)
        transform <- Transformation$new(dirPath)
    else if (file.exists(filePath))
    {
        convertImage <- function (rawImage)
        {
            if (!is.null(rawImage$source) && rawImage$source != "")
                return (rawImage$source)
            else if (!is.null(rawImage$data))
                return (deserialiseReferenceObject(object=rawImage))
            else
                report(OL$Error, "Old-style serialised transform file #{path} cannot be updated")
        }
        
        fields <- deserialiseReferenceObject(filePath, raw=TRUE)
        transform <- Transformation$new(dirPath, convertImage(fields$sourceImage), convertImage(fields$targetImage))
        transform$updateFromObjects(fields$affineMatrices, fields$controlPointImages, fields$reverseControlPointImages, fields$method)
    }
    else
        transform <- Transformation$new(dirPath, source, target)
    
    return (transform)
}

registerImages <- function (sourceImage, targetImage, transform = NULL, sourceMask = NULL, targetMask = NULL, method = getOption("tractorRegistrationMethod"), types = "affine", affineDof = 12, estimateOnly = FALSE, interpolation = 1, ...)
{
    if (is.null(method))
        method <- "niftyreg"
    else
        method <- match.arg(method, c("niftyreg","fsl"))
    types <- match.arg(types, c("affine","nonlinear","reverse-nonlinear"), several.ok=TRUE)
    
    if (!is.null(transform))
    {
        if (missing(sourceImage))
            sourceImage <- transform$getSourceImage()
        if (missing(targetImage))
            targetImage <- transform$getTargetImage()
    }
    else
        transform <- Transformation$new(threadSafeTempFile(), sourceImage, targetImage)
    
    if (method == "niftyreg")
        result <- registerImagesWithNiftyreg(transform, sourceMask=sourceMask, targetMask=targetMask, types=types, affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    else if (method == "fsl")
    {
        if (any(c("nonlinear","reverse-nonlinear") %in% types))
            report(OL$Error, "FSL-FLIRT does not perform nonlinear registration")
        
        result <- registerImagesWithFlirt(transform, sourceMaskFileName=getImageAsFileName(sourceMask,allowNull=TRUE), targetMaskFileName=getImageAsFileName(targetMask,allowNull=TRUE), affineDof=affineDof, estimateOnly=estimateOnly, interpolation=interpolation, ...)
    }
    
    return (result)
}

resampleImage <- function (image, voxelDims = NULL, imageDims = NULL, interpolation = 1)
{
    if (!is(image, "MriImage"))
        report(OL$Error, "Specified image is not a valid MriImage object")
    if (is.null(voxelDims) && is.null(imageDims))
        report(OL$Error, "Image or voxel dimensions must be given")
    
    if (is.null(voxelDims))
        voxelDims <- image$getFieldOfView() / imageDims
    scales <- abs(image$getVoxelDimensions() / voxelDims)
    
    xfm <- buildAffine(scales=scales, source=image)
    result <- applyTransform(xfm, image, interpolation=.interpolationNameToCode(interpolation))
    
    return (as(result, "MriImage"))
}

identityTransformation <- function (sourceImage, targetImage)
{
    transform <- Transformation$new(threadSafeTempFile(), sourceImage, targetImage)
    transform$updateFromObjects(affineMatrices=list(buildAffine(source=sourceImage,target=targetImage)))
    return (transform)
}

decomposeTransformation <- function (transform)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    if (!("affine" %in% transform$getTypes()))
        report(OL$Error, "Decomposition can only be performed for affine transformations")
    
    return (lapply(transform$getTransformObjects(1:transform$nRegistrations(),preferAffine=TRUE), decomposeAffine))
}

mergeTransformations <- function (transforms, newSourceImage)
{
    if (!is.list(transforms))
        report(OL$Error, "Transformations must be specified in a list")
    if (length(transforms) == 0)
        report(OL$Error, "At least one transformation must be given")
    
    methods <- sapply(transforms, function(x) x$getMethod())
    if (!all(methods == methods[1]))
        report(OL$Error, "Method must be the same for all transformations")
    
    targetDir <- threadSafeTempFile()
    dir.create(targetDir)
    writeLines(methods[1], file.path(targetDir,"method.txt"))
    
    offset <- 0L
    digits <- ore("\\d+")
    for (i in seq_along(transforms)[-1])
    {
        if (offset > 0L)
        {
            oldNames <- list.files(transforms[[i]]$getDirectory()) %~|% digits
            newNames <- ore.subst(digits, function(n) as.integer(n)+offset, oldNames)
            if (!all(file.copy(file.path(transforms[[i]]$getDirectory(),oldNames), file.path(targetDir,newNames))))
                report(OL$Error, "Couldn't copy transformation files to new directory")
        }
        offset <- offset + transforms[[i]]$nRegistrations()
    }
    
    transform <- Transformation$new(targetDir, newSourceImage, transforms[[1]]$getTargetImage())
    return (transform)
}

composeTransformations <- function (transforms)
{
    if (!is.list(transforms))
        report(OL$Error, "Transformations must be specified in a list")
    if (length(transforms) == 0)
        report(OL$Error, "At least one transformation must be given")
    
    transform <- Reduce(function(x,y) {
        n <- max(x$nRegistrations(), y$nRegistrations())
        xi <- rep(1:x$nRegistrations(), length.out=n)
        yi <- rep(1:y$nRegistrations(), length.out=n)
        xfms <- lapply(seq_len(n), function(i) composeTransforms(x$getTransformObjects(xi[i]), y$getTransformObjects(yi[i])))
        rxfms <- lapply(seq_len(n), function(i) composeTransforms(y$getTransformObjects(yi[i],reverse=TRUE,errorIfMissing=FALSE), x$getTransformObjects(xi[i],reverse=TRUE,errorIfMissing=FALSE)))
        z <- Transformation$new(threadSafeTempFile(), x$getSourceImage(), y$getTargetImage())
        z$updateFromObjects(xfms, xfms, rxfms, method="niftyreg")
    }, transforms)
    
    return (transform)
}
