#' The Streamline class
#'
#' This class represents a single tractography streamline: a piecewise-linear
#' path composed of a sequence of 3D points, along with the seed point it was
#' grown from and metadata about the image space it was generated in.
#'
#' @field line A numeric matrix with three columns, giving the (x,y,z)
#'   coordinates of each point on the line, one per row.
#' @field seedIndex An integer giving the index (row of `line`) corresponding
#'   to the seed point from which the streamline was generated.
#' @field spaceDims An integer vector of length 3 giving the dimensions of
#'   the image space that the streamline was generated within.
#' @field voxelDims A numeric vector of length 3 giving the voxel dimensions
#'   of the image space that the streamline was generated within.
#' @field coordUnit A string giving the unit that the coordinates in `line`
#'   are stored in, either `"vox"` (voxels) or `"mm"` (millimetres).
#' @field pointSpacings A numeric vector giving the Euclidean distance
#'   between each successive pair of points on the line. Has length one less
#'   than the number of points.
#'
#' @export
Streamline <- setRefClass("Streamline", contains="SerialisableObject", fields=list(line="matrix",seedIndex="integer",spaceDims="integer", voxelDims="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = emptyMatrix(), seedIndex = NULL, spaceDims = NULL, voxelDims = NULL, coordUnit = c("vox","mm"), pointSpacings = NULL, ...)
    {
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), spaceDims=as.integer(spaceDims)[1:3], voxelDims=as.numeric(voxelDims)[1:3], coordUnit=match.arg(coordUnit))

        if (nrow(object$line) != 0 && ncol(object$line) != 3)
            report(OL$Error, "Streamline must be specified as a matrix with 3 columns")
        
        if (!is.null(pointSpacings))
            object$pointSpacings <- rep(as.numeric(pointSpacings), length.out=nrow(object$line)-1)
        else if (nrow(object$line) < 2)
            object$pointSpacings <- numeric(0)
        else
            object$pointSpacings <- apply(diff(object$line), 1, vectorLength)
        
        return (object)
    },
    
    getCoordinateUnit = function () { return (coordUnit) },
    
    getLine = function (unit = NULL)
    {
        "Retrieve line coordinates, optionally converting between voxel and world (mm) units"
        if (is.null(unit))
            return (line)
        else
        {
            unit <- match.arg(unit, c("vox","mm"))
            if (unit == "vox" && .self$coordUnit == "mm")
                return (t(apply(line, 1, "/", abs(voxelDims))) + 1)
            else if (unit == "mm" && .self$coordUnit == "vox")
                return (t(apply(line-1, 1, "*", abs(voxelDims))))
            else
                return (line)
        }
    },
    
    getLineLength = function () { return (sum(pointSpacings)) },
    
    getPointSpacings = function () { return (pointSpacings) },
    
    getSeedIndex = function () { return (seedIndex) },
    
    getSeedPoint = function () { return (line[seedIndex,]) },
    
    getSpaceDimensions = function () { return (spaceDims) },
    
    getVoxelDimensions = function () { return (voxelDims) },
    
    nPoints = function () { return (nrow(line)) },
    
    setCoordinateUnit = function (newUnit = c("vox","mm"))
    {
        "Convert the line's point coordinates to a new unit system"
        newUnit <- match.arg(newUnit)
        .self$line <- .self$getLine(newUnit)
        
        if (newUnit != .self$coordUnit)
        {
            .self$updatePointSpacings()
            .self$coordUnit <- newUnit
        }
        
        invisible(.self)
    },
    
    setMaximumSpacing = function (maxSeparation)
    {
        "Trim the line to remove sections with excessively large gaps between consecutive points"
        if (length(.self$pointSpacings) > 0)
        {
            wide <- which(.self$pointSpacings > maxSeparation)
            leftWide <- wide[which(wide < seedIndex)]
            rightWide <- wide[which(wide > seedIndex)]
            
            # NB: pointSpacings[1] is the distance *from* the first point on the line
            leftStop <- ifelse(length(leftWide) > 0, max(leftWide)+1, 1)
            rightStop <- ifelse(length(rightWide) > 0, min(rightWide), .self$nPoints())
            
            .self$trim(leftStop, rightStop)
        }
        
        invisible(.self)
    },
    
    transform = function (xfm, reverse = FALSE, ...)
    {
        "Transform the line into a new image space"
        .self$line <- promote(tractor.reg::transformPoints(xfm, .self$line, voxel=(.self$coordUnit=="vox"), reverse=reverse, ...), byrow=TRUE)
        .self$updatePointSpacings()
        
        if (reverse)
            .self$voxelDims <- RNifti::pixdim(xfm$getSource())[1:3]
        else
            .self$voxelDims <- RNifti::pixdim(xfm$getTarget())[1:3]
        
        invisible(.self)
    },
    
    trim = function (start = NULL, end = NULL)
    {
        "Trim the line to the given range of points"
        if (is.null(start))
            start <- 1
        if (is.null(end))
            end <- nrow(line)
        
        if (start != 1 || end != nrow(line))
        {
            .self$line <- line[start:end,,drop=FALSE]
            .self$seedIndex <- as.integer(seedIndex - start + 1)
            .self$pointSpacings <- .self$pointSpacings[start-1 + seq_len(end-start)]
        }
    },
    
    updatePointSpacings = function ()
    {
        "Recalculate the spacing between consecutive points on the line"
        if (nrow(.self$line) < 2)
            .self$pointSpacings <- numeric(0)
        else
            .self$pointSpacings <- apply(diff(.self$line), 1, vectorLength)
        
        invisible(.self)
    }
))

#' The StreamlineSource class
#'
#' This class represents a source of tractography streamlines, which may be
#' an active [Tracker], a set of streamlines read from file, or a list of
#' [Streamline] objects already held in memory. It provides a common
#' interface for filtering, selecting and retrieving streamlines and
#' derived products, regardless of the underlying source. Objects of this
#' class are created by [generateStreamlines()], [readStreamlines()] or
#' [attachStreamlines()], rather than directly.
#'
#' @field type A string identifying the type of underlying source, such as
#'   `"tracker"`, `"file"` or `"list"`.
#' @field file A string giving the file stem associated with the source, if
#'   it corresponds to a file on disk, or the empty string otherwise.
#' @field selection An integer vector giving the indices of a subset of
#'   streamlines to operate on, or an empty vector if all streamlines are
#'   to be used.
#' @field count An integer giving the total number of streamlines available
#'   from the source.
#' @field labels A logical value indicating whether or not streamline labels
#'   are available.
#' @field properties A character vector naming the per-streamline scalar
#'   properties available from the source, if any.
#' @field pointer An external pointer to the underlying C++ pipeline object.
#'
#' @export
StreamlineSource <- setRefClass("StreamlineSource", contains="TractorObject", fields=list(type="character", file="character", selection="integer", count="integer",labels="logical", properties="character", pointer="externalptr"), methods=list(
    initialize = function (pointer = nilPointer(), fileStem = NULL, count = 0L, properties = NULL, labels = FALSE, ...)
    {
        initFields(file=as.character(fileStem), selection=integer(0), count=as.integer(count), labels=labels, properties=as.character(properties), pointer=pointer)
    },
    
    filter = function (minLabels = NULL, maxLabels = NULL, minLength = NULL, maxLength = NULL, medianOnly = FALSE, medianLengthQuantile = 0.99)
    {
        "Set filters restricting which streamlines will be produced by the source"
        .Call("setFilters", pointer, minLabels %||% 0L, maxLabels %||% 0L, minLength %||% 0, maxLength %||% 0, medianOnly, medianLengthQuantile, PACKAGE="tractor.track")
        invisible(.self)
    },
    
    getFileStem = function () { return (file) },
    
    getSelection = function () { return (selection) },
    
    getStreamlines = function (simplify = TRUE)
    {
        "Run the pipeline and retrieve the resulting streamlines"
        result <- .self$process()
        if (simplify && length(result$streamlines) == 1)
            return (result$streamlines[[1]])
        else
            return (result$streamlines)
    },
    
    getVisitationMap = function (scope = c("full","seed","ends"), normalise = FALSE, refImage = NULL)
    {
        "Run the pipeline and retrieve a streamline visitation map image"
        result <- .self$process(requireStreamlines=FALSE, requireMap=TRUE, mapScope=match.arg(scope), normaliseMap=normalise, refImage=refImage)
        return (result$map)
    },
    
    hasLabels = function () { return (labels) },
    
    matchLabels = function (labels, image = NULL, combine = c("none","and","or"))
    {
        "Find streamlines matching a given set of labels"
        combine <- match.arg(combine)
        .Call("trkFind", pointer, labels, image, combine, PACKAGE="tractor.track")
    },
    
    nStreamlines = function () { return (count) },
    
    process = function (path = NULL, requireStreamlines = TRUE, requireMap = FALSE, mapScope = c("full","seed","ends"), normaliseMap = FALSE, requireProfile = FALSE, requireLengths = FALSE, truncate = NULL, refImage = NULL, debug = 0L)
    {
        "Run the underlying streamline processing pipeline, producing any combination of streamlines, a visitation map, a label profile and streamline lengths"
        mapScope <- match.arg(mapScope)
        
        if (nilPointer(.self$pointer))
            report(OL$Error, "Streamline source pointer is not valid")
        
        result <- .Call("runPipeline", pointer, selection, path %||% "", requireStreamlines, requireMap, mapScope, normaliseMap, requireProfile, requireLengths, truncate$left, truncate$right, refImage, debug, Streamline$new, PACKAGE="tractor.track")
        
        # The map is a niftiImage, so convert it back to MriImage
        if (!is.null(result$map))
            result$map <- as(result$map, "MriImage")
        
        return (result)
    },
    
    select = function (indices = NULL)
    {
        .self$selection <- as.integer(indices)
        invisible(.self)
    },
    
    summarise = function ()
    {
        "Summarise the streamline source"
        if (length(file) == 1 && file != "")
            values <- c("Streamline source"=expandFileName(file))
        else
            values <- c("Streamline source"="internal")
        values <- c(values, "Number of streamlines"=count, "Streamline properties"=implode(properties,sep=", "), "Streamline labels"=labels)
        return (values)
    },
    
    writeStreamlines = function (path = threadSafeTempFile())
    {
        "Write streamlines to file"
        .self$process(path, requireStreamlines=TRUE)
        invisible(path)
    }
))

#' Create a streamline object
#'
#' Create a [Streamline] object from a matrix of points, checking and
#' completing metadata about the image space as required.
#'
#' @param line A numeric matrix with three columns, giving the (x,y,z)
#'   coordinates of each point on the line, one per row.
#' @param seedIndex An integer giving the index (row of `line`) of the seed
#'   point from which the streamline was generated.
#' @param spaceDims An integer vector of length 3 giving the dimensions of
#'   the image space that the streamline was generated within. If `NULL`,
#'   this and `voxelDims` are obtained from `image` instead.
#' @param voxelDims A numeric vector of length 3 giving the voxel dimensions
#'   of the image space that the streamline was generated within. If `NULL`,
#'   this and `spaceDims` are obtained from `image` instead.
#' @param image An image, or path to one, from which `spaceDims` and
#'   `voxelDims` can be obtained if they are not given explicitly.
#' @param coordUnit A string giving the unit that the coordinates in `line`
#'   are given in, either `"vox"` (voxels) or `"mm"` (millimetres).
#' @return A [Streamline] object.
#' @author Jon Clayden
#' @export
asStreamline <- function (line, seedIndex = NULL, spaceDims = NULL, voxelDims = NULL, image = NULL, coordUnit = c("vox","mm"))
{
    coordUnit <- match.arg(coordUnit)
    
    if (is.null(spaceDims) || is.null(voxelDims))
    {
        assert(!is.null(image), "Image and voxel dimensions or an image must be specified")
        image <- as(image, "MriImage")
        spaceDims <- spaceDims %||% image$getDimensions()
        voxelDims <- voxelDims %||% image$getVoxelDimensions()
    }
    
    streamline <- Streamline$new(line, seedIndex, spaceDims, voxelDims, coordUnit)
    invisible(streamline)
}

#' Generate streamlines from seed points
#'
#' Perform streamline tractography from a set of seed points, using a
#' previously created [Tracker].
#'
#' @param tracker A [Tracker] object.
#' @param seeds A numeric matrix with three columns, giving the (x,y,z)
#'   voxel coordinates of the seed points to track from, one per row.
#' @param countPerSeed An integer giving the number of streamlines to
#'   generate from each seed point.
#' @param rightwardsVector An optional numeric vector of length 3, used to
#'   disambiguate left-right symmetric seed neighbourhoods.
#' @param jitter Logical value: should seed points be randomly perturbed
#'   within their voxel before tracking?
#' @return A [StreamlineSource] object encapsulating the generated
#'   streamlines.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
generateStreamlines <- function (tracker, seeds, countPerSeed, rightwardsVector = NULL, jitter = TRUE)
{
    assert(inherits(tracker,"Tracker"), "The specified tracker is not valid")
    pointer <- .Call("initialiseTracker", tracker$getPointer(), promote(seeds,byrow=TRUE), countPerSeed, rightwardsVector, jitter, PACKAGE="tractor.track")
    source <- StreamlineSource$new(pointer, "", nrow(seeds)*countPerSeed)
    invisible(source)
}

#' Read streamlines from file
#'
#' Open a file of previously generated streamlines, in TrackVis (.trk) or
#' MRtrix (.tck) format, for subsequent processing.
#'
#' @param fileName A character string giving the path to the streamline
#'   file, with or without an appropriate extension.
#' @param readLabels Logical value: should streamline labels, if present,
#'   be read in along with the streamlines themselves?
#' @return A [StreamlineSource] object encapsulating the streamlines in the
#'   file.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
readStreamlines <- function (fileName, readLabels = TRUE)
{
    assert(length(fileName) == 1 && fileName != "", "A single file name should be specified")
    fileStem <- ensureFileSuffix(fileName, NULL, strip=c("tck","trk","trkl"))
    info <- .Call("trkOpen", fileStem, readLabels, PACKAGE="tractor.track")
    source <- StreamlineSource$new(info$pointer, fileStem, info$count, properties=info$properties, labels=info$labels)
    invisible(source)
}

#' Attach streamlines held in memory to a source
#'
#' Wrap one or more [Streamline] objects already held in memory in a
#' [StreamlineSource], so that they can be filtered, selected and processed
#' using the same interface as streamlines read from file or generated by
#' tracking.
#'
#' @param streamlines A [Streamline] object, or a list of them.
#' @return A [StreamlineSource] object encapsulating the given streamlines.
#' @author Jon Clayden
#' @export
attachStreamlines <- function (streamlines)
{
    if (!is.list(streamlines) && inherits(streamlines,"Streamline"))
        streamlines <- list(streamlines)
    pointer <- .Call("createListSource", streamlines, PACKAGE="tractor.track")
    source <- StreamlineSource$new(pointer, "", length(streamlines))
    invisible(source)
}
