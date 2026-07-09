#' The diffusionScheme class (S7)
#'
#' This S7 class represents a diffusion MRI acquisition scheme. It encapsulates
#' a series of diffusion-weighted volume acquisitions, each with an associated
#' diffusion-sensitising gradient direction, in one or more "shells" with a
#' given weighting ("b-value"). This is a value-semantics replacement for the
#' \code{\linkS4class{DiffusionScheme}} reference class, which is now a thin
#' backwards-compatible shell around this class.
#'
#' @param bValues A vector of b-values in seconds per square millimetre, one
#'   per volume acquired.
#' @param gradientDirections A matrix of gradient directions, one row per
#'   volume acquired.
#' @param shellIndices An integer vector giving the shell index associated
#'   with each volume.
#' @param shellValues A vector of b-values associated with each shell.
#'
#' @export
diffusionScheme <- S7::new_class("diffusionScheme", package = NULL, properties = list(
    bValues = S7::class_numeric,
    gradientDirections = S7::new_S3_class("matrix"),
    shellIndices = S7::class_integer,
    shellValues = S7::class_numeric),
    validator = function(self) {
        if (length(self@bValues) > 0 && length(self@bValues) != nrow(self@gradientDirections))
            return ("Gradient matrix doesn't match the length of the b-value vector")
        if (length(self@bValues) != length(self@shellIndices))
            return ("Shell indices don't match the length of the b-value vector")
        NULL
    })

S7::S4_register(diffusionScheme)

#' Create a diffusionScheme object
#'
#' Constructor helper used by both the S7 constructor and the legacy
#' \code{\linkS4class{DiffusionScheme}} shell. Performs the same shell
#' assignment and normalisation logic as the original reference class's
#' \code{initialize} method.
#'
#' @param bValues,directions,unweightedThreshold,tolerance As for the
#'   original \code{DiffusionScheme} class.
#' @return A \code{diffusionScheme} object.
#' @export
newDiffusionScheme <- function (bValues = NULL, directions = emptyMatrix(), unweightedThreshold = 100, tolerance = 0.02)
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
    assert(any(is.na(indices)), "Diffusion scheme contains only unweighted volumes at b-value threshold of #{unweightedThreshold}", level=OL$Warning)
    assert(any(!is.na(indices)), "Diffusion scheme contains no unweighted volumes at b-value threshold of #{unweightedThreshold}", level=OL$Warning)

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

    diffusionScheme(bValues=as.numeric(bValues), gradientDirections=unname(directions), shellIndices=indices, shellValues=values)
}

#' @export
print.diffusionScheme <- function (x, ...)
{
    summary <- .diffusionSchemeSummarise(x)
    printLabelledValues(summary$labels, summary$values)
    invisible(x)
}

#' @export
dim.diffusionScheme <- function (x) c(length(x@bValues), 3L)

#' @rdname accessors
#' @export
bValues <- S7::new_generic("bValues", "x")

#' @export
method(bValues, diffusionScheme) <- function (x) x@bValues

#' @rdname accessors
#' @export
gradientDirections <- S7::new_generic("gradientDirections", "x")

#' @export
method(gradientDirections, diffusionScheme) <- function (x) x@gradientDirections

#' @rdname accessors
#' @export
shellIndices <- S7::new_generic("shellIndices", "x")

#' @export
method(shellIndices, diffusionScheme) <- function (x) x@shellIndices

#' @rdname accessors
#' @export
shellValues <- S7::new_generic("shellValues", "x")

#' @export
method(shellValues, diffusionScheme) <- function (x) x@shellValues

#' Number of shells and directions in a diffusion scheme
#'
#' @param x A \code{diffusionScheme} object.
#' @name shellCounts
#' @rdname shellCounts
#' @export
nShells <- S7::new_generic("nShells", "x")

#' @export
method(nShells, diffusionScheme) <- function (x) max(x@shellIndices)

#' @rdname shellCounts
#' @export
nDirections <- S7::new_generic("nDirections", "x")

#' @export
method(nDirections, diffusionScheme) <- function (x) table(x@shellValues[x@shellIndices+1L])

#' @export
method(flip, diffusionScheme) <- function (x, dims)
{
    dims <- intersect(as.integer(dims), 1:3)
    if (length(dims) > 0L)
    {
        newDirections <- x@gradientDirections
        newDirections[,dims] <- -newDirections[,dims]
        return (set_props(x, gradientDirections=newDirections))
    }
    else
        return (x)
}

#' Rotate the gradient directions of a diffusion scheme
#'
#' Named \code{rotateGradients()} rather than \code{rotate()} to avoid
#' colliding with the well-established \code{RNiftyReg::rotate()} (image
#' rotation) - both packages are commonly loaded together, and a bare
#' \code{rotate()} call after \code{library(tractor.reg)} would otherwise
#' resolve unpredictably depending on import order.
#'
#' @param x A \code{diffusionScheme} object.
#' @param rotation A 3x3 rotation matrix.
#' @export
rotateGradients <- S7::new_generic("rotateGradients", "x")

#' @export
method(rotateGradients, diffusionScheme) <- function (x, rotation)
{
    assert(equivalent(dim(rotation), c(3L,3L)) && equivalent(det(rotation), 1), "Argument does not look like a 3D rotation matrix")
    set_props(x, gradientDirections=t(rotation %*% t(x@gradientDirections)))
}

.diffusionSchemeSummarise <- function (x)
{
    labels <- c("Number of shells", "Diffusion b-values", "Number of directions")
    values <- c(nShells(x), paste(implode(shellValues(x),", "), "s/mm^2"), implode(nDirections(x),", "))
    return (list(labels=labels, values=values))
}
