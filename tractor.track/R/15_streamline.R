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
    
    apply = function (fun, ..., simplify = TRUE)
    {
        fun <- match.fun(fun)
        n <- ifelse(length(selection) == 0, count, length(selection))
        if (!is.na(simplify))
        {
            results <- vector("list", n)
            i <- 1
        }
        
        .applyFunction <- function (points, seedIndex, voxelDims, coordUnit)
        {
            streamline <- Streamline$new(points, seedIndex, voxelDims, coordUnit)
            if (is.na(simplify))
                fun(streamline, ...)
            else
            {
                results[[i]] <<- fun(streamline, ...)
                i <<- i + 1
            }
        }
        
        .Call("trkApply", pointer, selection, .applyFunction, PACKAGE="tractor.track")
        
        if (isTRUE(simplify) && n == 1)
            return (results[[1]])
        else if (!is.na(simplify))
            return (results)
    },
    
    extractAndTruncate = function (leftLength, rightLength)
    {
        tempFile <- threadSafeTempFile()
        .Call("trkTruncate", pointer, selection, tempFile, leftLength, rightLength, PACKAGE="tractor.track")
        return (StreamlineSource$new(tempFile))
    },
    
    filter = function (minLabels = NULL, maxLabels = NULL, minLength = NULL, maxLength = NULL, medianOnly = FALSE, medianLengthQuantile = 0.99)
    {
        .Call("setFilters", pointer, minLabels %||% 0L, maxLabels %||% 0L, minLength %||% 0, maxLength %||% 0, medianOnly, medianLengthQuantile, PACKAGE="tractor.track")
        invisible(.self)
    },
    
    getFileStem = function () { return (file) },
    
    getLengths = function ()
    {
        return (.Call("trkLengths", pointer, selection, PACKAGE="tractor.track"))
    },
    
    getMapAndLengthData = function ()
    {
        return (.Call("trkFastMapAndLengths", pointer, selection, PACKAGE="tractor.track"))
    },
    
    getMedian = function (quantile = 0.99, pathOnly = FALSE)
    {
        tempFile <- threadSafeTempFile()
        .Call("trkMedian", pointer, selection, tempFile, quantile, PACKAGE="tractor.track")
        
        if (pathOnly)
            return (tempFile)
        else
            return (StreamlineSource$new(tempFile)$getStreamlines())
    },
    
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
    
    nStreamlines = function () { return (count) },
    
    process = function (path = NULL, requireStreamlines = TRUE, requireMap = FALSE, mapScope = c("full","seed","ends"), normaliseMap = FALSE, requireProfile = FALSE, requireLengths = FALSE, truncate = NULL, refImage = NULL, debug = 0L)
    {
        mapScope <- match.arg(mapScope)
        
        if (nilPointer(.self$pointer))
            report(OL$Error, "")
        
        .Call("runPipeline", pointer, selection, path %||% "", requireStreamlines, requireMap, mapScope, normaliseMap, requireProfile, requireLengths, truncate$left, truncate$right, refImage, debug, Streamline$new)
    },
    
    select = function (indices = NULL, labels = NULL)
    {
        if (is.null(indices) && is.null(labels))
            .self$selection <- integer(0)
        else
        {
            if (is.null(indices))
                indices <- .Call("trkFind", pointer, as.integer(labels), PACKAGE="tractor.track")
            .self$selection <- as.integer(indices)
        }
        
        invisible(.self)
    },
    
    summarise = function ()
    {
        if (length(file) == 1 && file != "")
            values <- c("Streamline source"=file)
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

setClassUnion("MriImageOrNull", c("MriImage","NULL"))

StreamlineSink <- setRefClass("StreamlineSink", fields=list(file="character",mask="MriImageOrNull",sinkPtr.="externalptr"), methods=list(
    initialize = function (file = NULL, mask = NULL, ...)
    {
        if (is.null(file) || is.null(mask))
            report(OL$Error, "Streamline source file and mask must be specified")
        
        file <- ensureFileSuffix(file, NULL, strip=c("trk","trkl"))
        
        return (initFields(file=file, mask=mask, sinkPtr.=NULL))
    },
    
    append = function (streamline)
    {
        if (is.null(sinkPtr.))
            .self$sinkPtr. <- .Call("trkCreate", file, mask, PACKAGE="tractor.track")
        
        if (is(streamline, "Streamline"))
        {
            if (!equivalent(streamline$getVoxelDimensions(), mask$getVoxelDimensions()))
                report(OL$Error, "Streamline voxel dimensions do not match the reference image")
            
            fixedSpacings <- all(streamline$getPointSpacings()[1] == streamline$getPointSpacings())
            .Call("trkAppend", sinkPtr., streamline$getLine(), streamline$getSeedIndex(), streamline$getCoordinateUnit(), fixedSpacings, PACKAGE="tractor.track")
        }
        
        invisible(.self)
    },
    
    close = function ()
    {
        if (!is.null(sinkPtr.))
        {
            .Call("trkClose", sinkPtr., PACKAGE="tractor.track")
            .self$sinkPtr. <- NULL
        }
    }
))

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
