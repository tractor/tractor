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

readDiffusionScheme <- function (fileName, ...)
{
    bValues <- directions <- NULL
    if (file.exists(fileName))
    {
        directions <- readMatrix(fileName)
        if (basename(fileName) == "bvecs")
            bValues <- readMatrix(file.path(dirname(fileName), "bvals"))
    }
    else
    {
        candidateFiles <- ensureFileSuffix(fileName, c("dirs","bval","bvec"))
        if (file.exists(candidateFiles[1]))
            directions <- readMatrix(candidateFiles[1])
        else if (all(file.exists(candidateFiles[2:3])))
        {
            bValues <- readMatrix(candidateFiles[2])
            directions <- readMatrix(candidateFiles[3])
        }
    }
    
    if (NCOL(directions) == 4L)
    {
        bValues <- directions[,4]
        directions <- directions[,1:3]
    }
    if (is.null(bValues) || is.null(directions))
        output(OL$Error, "No diffusion scheme found for path #{fileName}")
    
    bValues[is.na(bValues)] <- 0
    directions[is.na(directions)] <- 0
    return (DiffusionScheme$new(drop(bValues), directions, ...))
}

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
