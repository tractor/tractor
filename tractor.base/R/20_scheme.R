#' The DiffusionScheme class
#' 
#' This class represents a diffusion MRI acquisition scheme. It encapsulates a
#' series of diffusion-weighted volume acquisitions, each with an associated
#' diffusion-sensitising gradient direction, in one or more "shells" with a
#' given weighting ("b-value").
#' 
#' @field bValues A vector of b-values in seconds per square millimetre, one
#'   per volume acquired.
#' @field gradientDirections A matrix of gradient directions, one row per
#'   volume acquired. Columns give the x, y and z components of the directions,
#'   relative to the image axes.
#' @field shellIndices An integer vector giving the shell index associated with
#'   each volume. This is calculated internally and coded such that every
#'   volume with a b-value below \code{unweightedThreshold} gets an index of 0,
#'   and then others are grouped into shells with b-values differing by less
#'   than the relative \code{tolerance} specified (2% by default). This
#'   grouping is indicative only, and not stored in direction files.
#' @field shellValues A vector of b-values associated with each shell. These
#'   are calculated as the mode of the assigned volume b-values in each case.
#' 
#' @export
DiffusionScheme <- setRefClass("DiffusionScheme", contains="SerialisableObject", fields=list(bValues="numeric",gradientDirections="matrix", shellIndices="integer",shellValues="numeric"), methods=list(
    initialize = function (bValues = NULL, directions = emptyMatrix(), unweightedThreshold = 100, tolerance = 0.02, ...)
    {
        if (is.list(directions))
            directions <- t(Reduce(cbind, directions))
        
        assert(is.matrix(directions), "Gradient directions must be specified in a matrix")
        assert(any(dim(directions) == 3L), "Gradient matrix does not seem to be 3D")
        
        # NB: This changed in TractoR 3.0, from column-per-direction to row-per-direction
        if (ncol(directions) != 3 && nrow(directions) == 3)
        {
            report(OL$Verbose, "Transposing gradient direction matrix")
            directions <- t(directions)
        }
        
        assert(length(bValues) == nrow(directions), "Gradient matrix doesn't match the length of the b-value vector")
        
        # Normalise directions to unit length
        directions <- t(apply(directions, 1, function (x) {
            length <- vectorLength(x)
            return (x / ifelse(length==0,1,length))
        }))
        
        # Assign all volumes with b-values below threshold to shell 0, and check that this is neither all nor none of the volumes
        indices <- rep(NA_integer_, length(bValues))
        indices[bValues < unweightedThreshold] <- 0L
        assert(any(is.na(indices)), "Diffusion scheme contains only unweighted volumes at b-value threshold of #{threshold}", level=OL$Warning)
        assert(any(!is.na(indices)), "Diffusion scheme contains no unweighted volumes at b-value threshold of #{threshold}", level=OL$Warning)
        
        # Find the modal unweighted value, or default to zero
        if (any(!is.na(indices)))
            values <- as.numeric(names(which.max(table(bValues[!is.na(indices)]))))
        else
            values <- 0
        
        # Assign remaining volumes to shells with the most common b-value within tolerance
        allCounts <- sort(table(round(bValues)), decreasing=TRUE)
        allValues <- as.integer(names(allCounts))
        allValues <- allValues[allValues >= unweightedThreshold]
        currentIndex <- 1L
        for (value in allValues)
        {
            # Only create a new shell if there are suitable unassigned values available
            matches <- is.na(indices) & abs(bValues - value) / value < tolerance
            if (!any(matches))
                next
            values <- c(values, value)
            indices[matches] <- currentIndex
            
            # If every volume is assigned, we're done
            if (!any(is.na(indices)))
                break
            currentIndex <- currentIndex + 1L
        }
        
        # Order shells by ascending b-value
        shellOrder <- order(values)
        indices <- structure(shellOrder[indices+1L] - 1L)
        values <- values[shellOrder]
        
        initFields(bValues=as.numeric(bValues), gradientDirections=unname(directions), shellIndices=indices, shellValues=values)
    },
    
    flip = function (dims)
    {
        dims <- intersect(as.integer(dims), 1:3)
        if (length(dims) > 0L)
            .self$gradientDirections[,dims] <- -gradientDirections[,dims]
        invisible(.self)
    },
    
    getBValues = function () { return (bValues) },
    
    getGradientDirections = function () { return (gradientDirections) },
    
    getShellBValues = function () { return (shellValues) },
    
    getShellIndices = function () { return (shellIndices) },
    
    nDirections = function () { return (table(shellValues[shellIndices+1L])) },
    
    nShells = function () { return (max(shellIndices)) },
    
    nVolumes = function () { return (length(bValues)) },
    
    rotate = function (rotation)
    {
        assert(equivalent(dim(rotation), c(3L,3L)) && equivalent(det(rotation), 1), "Argument does not look like a 3D rotation matrix")
        .self$gradientDirections <- t(rotation %*% t(gradientDirections))
        invisible(.self)
    },
    
    summarise = function ()
    {
        labels <- c("Number of shells", "Diffusion b-values", "Number of directions")
        values <- c(nShells(), paste(implode(getShellBValues(),", "), "s/mm^2"), implode(nDirections(),", "))
        return (list(labels=labels, values=values))
    },
    
    writeToFile = function (fileName)
    {
        writeDiffusionScheme(.self, fileName)
        invisible(.self)
    }
))

#' Create a DiffusionScheme object from data
#' 
#' This is a generic function that converts another object to a
#' \code{DiffusionScheme} by extracting the relevant information. There are
#' methods for matrices (of gradient directions) and \code{MriImage} objects.
#' An image passed as an argument to the latter must have the \code{bValues}
#' and \code{bVectors} tags set appropriately.
#' 
#' @param x An object to coerce to a \code{DiffusionScheme}.
#' @param ... Additional arguments to methods.
#' @param bValues A vector of b-values, required when \code{x} is a matrix of
#'   gradient directions.
#' @return A \code{DiffusionScheme} object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
asDiffusionScheme <- function (x, ...)
{
    UseMethod("asDiffusionScheme")
}

#' @export
asDiffusionScheme.matrix <- function (x, bValues, ...)
{
    return (DiffusionScheme$new(bValues, x))
}

#' @export
asDiffusionScheme.MriImage <- function (x, ...)
{
    assert(all(x$hasTags(c("bValues","bVectors"))), "Image does not contain b-value and diffusion direction metadata")
    return (DiffusionScheme$new(x$getTags("bValues"), x$getTags("bVectors")))
}

#' @export
asDiffusionScheme.DiffusionScheme <- function (x, ...) { return (x) }

readMatrix <- function (fileName) as.matrix(read.table(fileName))

writeMatrix <- function (matrix, fileName, missing = NA)
{
    if (!is.na(missing))
        matrix[is.na(matrix)] <- missing
    # Never use scientific notation; drop decimal point and tailing zeroes for integer-valued elements
    lines <- apply(format(matrix,scientific=FALSE), 1, implode, sep="  ")
    lines <- ore.subst("\\.0+\\s*$", "", lines)
    writeLines(lines, fileName)
}

#' Read and write diffusion schemes
#' 
#' These functions read diffusion acquisition scheme objects from, and write
#' them to, text-based matrix files. They can handle both FSL-style (two-file)
#' and TractoR-style (single file) formats; in the two-file case either file
#' name can be specified.
#' 
#' Three main naming conventions for these files are recognised. TractoR's
#' preferred format is a single file with a ".dirs" extension, with the 
#' direction information stored one volume per row and b-values in the final
#' column. FSL uses files called "bvecs" and "bvals", with values stored one
#' volume per column; if the specified basic file name is exactly one of these
#' two then this format is assumed. BIDS uses the same format, but in files
#' named for the associated image with ".bvec" and ".bval" extensions. Any
#' other naming convention can be forced when writing by wrapping the file
#' name in a call to \code{\link{I}}, the "as-is" function.
#' 
#' @param fileName A string specifying a file name or stem.
#' @param bValues A numeric vector of b-values, or a single string naming a
#'   file containing them (in a format readable by \code{\link{read.table}}).
#'   The default is \code{NULL}, meaning that they should be read from the
#'   \code{fileName} or a sidecar "bval" file.
#' @param imagePath An optional string giving the path to an image. If fewer
#'   b-values are given than the number of gradient directions in the file,
#'   the image will be read and passed to \code{\link{fillShells}} to fill in
#'   the gaps. This is a convenience feature, but is heuristic-based and so
#'   may not always be reliable.
#' @param ... Further arguments to the \code{\linkS4class{DiffusionScheme}}
#'   constructor, to adjust how shells are interpreted.
#' @param scheme A \code{\linkS4class{DiffusionScheme}} object to write to
#'   file.
#' @return \code{readDiffusionScheme} returns a \code{DiffusionScheme} object,
#'   or \code{NULL} if one cannot be read. \code{writeDiffusionScheme} is
#'   called for its side effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
readDiffusionScheme <- function (fileName, bValues = NULL, imagePath = NULL, ...)
{
    directions <- NULL
    if (is.character(bValues) && length(bValues) == 1L && file.exists(bValues))
        bValues <- readMatrix(bValues)
    
    if (file.exists(fileName))
    {
        directions <- readMatrix(fileName)
        if (basename(fileName) == "bvecs")
            bValues <- bValues %||% readMatrix(file.path(dirname(fileName), "bvals"))
    }
    else
    {
        candidateFiles <- ensureFileSuffix(fileName, c("dirs","bval","bvec"))
        if (file.exists(candidateFiles[1]))
            directions <- readMatrix(candidateFiles[1])
        else if (all(file.exists(candidateFiles[2:3])))
        {
            bValues <- bValues %||% readMatrix(candidateFiles[2])
            directions <- readMatrix(candidateFiles[3])
        }
    }
    
    # The b-values may be combined with the directions
    if (NCOL(directions) == 4L)
    {
        bValues <- bValues %||% directions[,4]
        directions <- directions[,1:3]
    }
    
    # By now both should have been identified
    # TODO: make this optionally an error
    if (is.null(bValues) || is.null(directions))
        return (NULL)
    
    # Handle mismatches between the number of b-values and directions using the supplied image
    # If this doesn't apply, DiffusionScheme's constructor will produce an error when it checks its arguments
    if (length(bValues) != nrow(directions) && !is.null(imagePath))
    {
        # Expand the b-values based on mean volume intensities
        report(OL$Info, "Reading image data to estimate shell structure")
        bValues <- fillShells(readImageFile(imagePath), unique(bValues))
        
        # If there are more image volumes than directions, specifically because b=0 volumes are omitted, we can patch that up by adding zero rows
        if (length(bValues) > nrow(directions) && sum(bValues != 0) == nrow(directions))
        {
            report(OL$Info, "Inserting zero-magnitude directions for b=0 volumes")
            allDirections <- matrix(0, nrow=length(bValues), ncol=3L)
            allDirections[bValues != 0,] <- directions
            directions <- allDirections
        }
    }
    
    bValues[is.na(bValues)] <- 0
    directions[is.na(directions)] <- 0
    return (DiffusionScheme$new(drop(bValues), directions, ...))
}

#' @rdname readDiffusionScheme
#' @export
writeDiffusionScheme <- function (scheme, fileName)
{
    files <- c(bValues=NA_character_, directions=NA_character_)
    candidateFiles <- ensureFileSuffix(fileName, c("dirs","bval","bvec"))
    if (fileName %in% candidateFiles[2:3])
        files <- c(bValues=candidateFiles[2], directions=candidateFiles[3])
    else if (basename(fileName) %in% c("bvals","bvecs"))
    {
        dir <- dirname(fileName)
        files <- c(bValues=file.path(dir,"bvals"), directions=file.path(dir,"bvecs"))
    }
    else if (inherits(fileName, "AsIs"))
        files["directions"] <- fileName
    else
        files["directions"] <- candidateFiles[1]
    
    # If there's an image at the target path that doesn't match the scheme, produce a warning
    fileStem <- ensureFileSuffix(fileName, NULL)
    if (!inherits(fileName,"AsIs") && imageFileExists(fileStem))
    {
        metadata <- readImageFile(fileStem, metadataOnly=TRUE)
        if (metadata$getDimensionality() != 4L)
            report(OL$Warning, "Image matching scheme path #{fileStem} is not 4D")
        else if (metadata$getDimensions()[4L] != scheme$nVolumes())
            report(OL$Warning, "Image matching scheme path #{fileStem} does not have the expected number of volumes")
    }
    
    if (is.na(files["bValues"]))
    {
        data <- cbind(scheme$getGradientDirections(), scheme$getBValues())
        writeMatrix(data, files["directions"], missing=0)
    }
    else
    {
        writeMatrix(t(scheme$getGradientDirections()), files["directions"], missing=0)
        writeMatrix(promote(scheme$getBValues(),byrow=TRUE), files["bValues"], missing=0)
    }
}

#' Assign image volumes to shells
#' 
#' This function guesses the assignment of image volumes to shells, expanding
#' a list of unique b-values to one value per volume. This is achieved by
#' performing k-means clustering on the mean intensities of each volume. The
#' set of unique b-values must be known beforehand, and no sanity checking is
#' performed on the intensities for each shell except ordering.
#' 
#' @param image A 4D diffusion-weighted image, whose mean volume intensities
#'   will be used to cluster the acquisition into shells. It does not need to
#'   already have gradient direction or b-value information associated with it.
#' @param bValues A numeric vector of unique b-values.
#' @return A numeric vector expanding the original \code{bValues} to one value
#'   per volume of the \code{image}.
#' 
#' @note The procedure performed by this function is a heuristic, and may
#'   produce inaccurate results if some shell b-values are close together, if
#'   there are many unique b-values, if the assumption of decreasing mean
#'   intensity with increasing b-value doesn't hold, etc. If the full
#'   acquisition scheme is known then it is alway better to specify it
#'   explicitly.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
fillShells <- function (image, bValues)
{
    if (!is(image, "MriImage"))
        image <- asMriImage(image)
    
    assert(image$getDimensionality() == 4L, "Specified image is not 4D")
    nVolumes <- image$getDimensions()[4L]
    
    bValues <- as.numeric(bValues)
    assert(length(bValues) <= nVolumes, "#{length(bValues)} b-values were specified, but the image only has #{nVolumes} volumes")
    
    if (length(bValues) < nVolumes)
    {
        shellValues <- sort(unique(bValues))
        nShells <- length(shellValues)
        
        meanIntensities <- image$apply(4, mean, na.rm=TRUE)
        kMeansResult <- kmeans(meanIntensities, nShells, nstart=3L)
        
        # Increasing b-value corresponds to decreasing image intensity
        bValues <- rep(NA, nVolumes)
        shellCounts <- integer(0L)
        clusterOrder <- order(kMeansResult$centers, decreasing=TRUE)
        for (i in seq_len(nShells))
        {
            indices <- which(kMeansResult$cluster == clusterOrder[i])
            shellCounts <- c(shellCounts, length(indices))
            bValues[indices] <- shellValues[i]
        }
        
        report(OL$Info, "Implied shell sizes are #{implode(shellCounts,', ')} for b-values of #{implode(shellValues,', ')} s/mm^2")
    }
    
    return (bValues)
}
