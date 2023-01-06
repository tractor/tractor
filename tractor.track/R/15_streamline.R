Streamline <- setRefClass("Streamline", contains="SerialisableObject", fields=list(line="matrix",seedIndex="integer",spaceDims="integer", voxelDims="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = emptyMatrix(), seedIndex = NULL, spaceDims = NULL, voxelDims = NULL, coordUnit = c("vox","mm"), pointSpacings = NULL, ...)
    {
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), spaceDims=as.integer(spaceDims)[1:3], voxelDims=as.numeric(voxelDims)[1:3], coordUnit=match.arg(coordUnit))

        if (ncol(object$line) != 3)
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
        .self$line <- promote(tractor.reg::transformPoints(xfm, .self$line, voxel=(.self$coordUnit=="vox"), reverse=reverse, ...), byrow=TRUE)
        .self$updatePointSpacings()
        
        if (reverse)
            .self$voxelDims <- xfm$getSourceImage(metadataOnly=TRUE)$getVoxelDimensions()[1:3]
        else
            .self$voxelDims <- xfm$getTargetImage(metadataOnly=TRUE)$getVoxelDimensions()[1:3]
        
        invisible(.self)
    },
    
    trim = function (start = NULL, end = NULL)
    {
        if (is.null(start))
            start <- 1
        if (is.null(end))
            end <- nrow(line)
        
        if (start != 1 || end != nrow(line))
        {
            .self$line <- line[start:end,]
            .self$seedIndex <- as.integer(seedIndex - start + 1)
            .self$pointSpacings <- .self$pointSpacings[start:(end-1)]
        }
    },
    
    updatePointSpacings = function ()
    {
        if (nrow(.self$line) < 2)
            .self$pointSpacings <- numeric(0)
        else
            .self$pointSpacings <- apply(diff(.self$line), 1, vectorLength)
        
        invisible(.self)
    }
))

StreamlineSource <- setRefClass("StreamlineSource", fields=list(type="character", file="character", selection="integer", count="integer",labels="logical", properties="character", pointer="externalptr"), methods=list(
    initialize = function (pointer = nilPointer(), fileStem = NULL, count = 0L, properties = NULL, labels = FALSE, ...)
    {
        initFields(file=as.character(fileStem), selection=integer(0), count=as.integer(count), labels=labels, properties=as.character(properties), pointer=pointer)
    },
    
    filter = function (minLabels = NULL, maxLabels = NULL, minLength = NULL, maxLength = NULL, medianOnly = FALSE, medianLengthQuantile = 0.99)
    {
        .Call("setFilters", pointer, minLabels %||% 0L, maxLabels %||% 0L, minLength %||% 0, maxLength %||% 0, medianOnly, medianLengthQuantile, PACKAGE="tractor.track")
        invisible(.self)
    },
    
    getFileStem = function () { return (file) },
    
    getSelection = function () { return (selection) },
    
    getStreamlines = function (simplify = TRUE)
    {
        result <- .self$process()
        if (simplify && length(result$streamlines) == 1)
            return (result$streamlines[[1]])
        else
            return (result$streamlines)
    },
    
    getVisitationMap = function (scope = c("full","seed","ends"), normalise = FALSE, refImage = NULL)
    {
        result <- .self$process(requireStreamlines=FALSE, requireMap=TRUE, mapScope=match.arg(scope), normaliseMap=normalise, refImage=refImage)
        return (result$map)
    },
    
    hasLabels = function () { return (labels) },
    
    matchLabels = function (labels, image = NULL, combine = c("none","and","or"))
    {
        combine <- match.arg(combine)
        .Call("trkFind", pointer, labels, image, combine, PACKAGE="tractor.track")
    },
    
    nStreamlines = function () { return (count) },
    
    process = function (path = NULL, requireStreamlines = TRUE, requireMap = FALSE, mapScope = c("full","seed","ends"), normaliseMap = FALSE, requireProfile = FALSE, requireLengths = FALSE, truncate = NULL, refImage = NULL, debug = 0L)
    {
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
        if (length(file) == 1 && file != "")
            values <- c("Streamline source"=expandFileName(file))
        else
            values <- c("Streamline source"="internal")
        values <- c(values, "Number of streamlines"=count, "Streamline properties"=implode(properties,sep=", "), "Streamline labels"=labels)
        return (values)
    },
    
    writeStreamlines = function (path = threadSafeTempFile())
    {
        .self$process(path, requireStreamlines=TRUE)
        invisible(path)
    }
))

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

generateStreamlines <- function (tracker, seeds, countPerSeed, rightwardsVector = NULL, jitter = TRUE)
{
    assert(inherits(tracker,"Tracker"), "The specified tracker is not valid")
    pointer <- .Call("initialiseTracker", tracker$getPointer(), promote(seeds,byrow=TRUE), countPerSeed, rightwardsVector, jitter, PACKAGE="tractor.track")
    source <- StreamlineSource$new(pointer, "", nrow(seeds)*countPerSeed)
    invisible(source)
}

readStreamlines <- function (fileName, readLabels = TRUE)
{
    assert(length(fileName) == 1 && fileName != "", "A single file name should be specified")
    fileStem <- ensureFileSuffix(fileName, NULL, strip=c("tck","trk","trkl"))
    info <- .Call("trkOpen", fileStem, readLabels, PACKAGE="tractor.track")
    source <- StreamlineSource$new(info$pointer, fileStem, info$count, properties=info$properties, labels=info$labels)
    invisible(source)
}

attachStreamlines <- function (streamlines)
{
    if (!is.list(streamlines) && inherits(streamlines,"Streamline"))
        streamlines <- list(streamlines)
    pointer <- .Call("createListSource", streamlines, PACKAGE="tractor.track")
    source <- StreamlineSource$new(pointer, "", length(streamlines))
    invisible(source)
}
