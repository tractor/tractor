Streamline <- setRefClass("Streamline", contains="SerialisableObject", fields=list(line="matrix",seedIndex="integer",voxelDims="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = matrix(NA,0,3), seedIndex = NULL, voxelDims = NULL, coordUnit = c("vox","mm"), pointSpacings = NULL, ...)
    {
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), voxelDims=as.numeric(voxelDims)[1:3], coordUnit=match.arg(coordUnit))

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
        if (nrow(.self$line) == 1)
            .self$pointSpacings <- numeric(0)
        else
            .self$pointSpacings <- apply(diff(.self$line), 1, vectorLength)
        
        invisible(.self)
    }
))

setClassUnion("ExternalPointerOrNull", c("externalptr","NULL"))

StreamlineSource <- setRefClass("StreamlineSource", fields=list(file="character",selection="integer",count.="integer",labelsPtr.="ExternalPointerOrNull"), methods=list(
    initialize = function (file = NULL, ...)
    {
        assert(!is.null(file), "Streamline source file must be specified")
        
        fileStem <- ensureFileSuffix(file, NULL, strip=c("tck","trk","trkl"))
        if (file.exists(ensureFileSuffix(fileStem, "trk")))
            count <- as.integer(.Call("trkCount", fileStem, PACKAGE="tractor.track"))
        else if (file.exists(ensureFileSuffix(fileStem, "tck")))
            count <- as.integer(.Call("tckCount", fileStem, PACKAGE="tractor.track"))
        else
            report(OL$Error, "Specified streamline source file does not exist")
        
        labelsPtr <- NULL
        if (file.exists(ensureFileSuffix(fileStem, "trkl")))
            labelsPtr <- .Call("trkLabels", fileStem, PACKAGE="tractor.track")
        
        return (initFields(file=fileStem, selection=integer(0), count.=count, labelsPtr.=labelsPtr))
    },
    
    apply = function (fun, ..., simplify = TRUE)
    {
        fun <- match.fun(fun)
        n <- ifelse(length(selection) == 0, count., length(selection))
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
        
        if (file.exists(ensureFileSuffix(file, "trk")))
            .Call("trkApply", file, selection, .applyFunction, PACKAGE="tractor.track")
        else if (file.exists(ensureFileSuffix(file, "tck")))
            .Call("tckApply", file, selection, .applyFunction, PACKAGE="tractor.track")
        else
            report(OL$Error, "Streamline source file no longer exists")
        
        if (isTRUE(simplify) && n == 1)
            return (results[[1]])
        else if (!is.na(simplify))
            return (results)
    },
    
    extractAndTruncate = function (leftLength, rightLength)
    {
        tempFile <- threadSafeTempFile()
        .Call("trkTruncate", file, selection, tempFile, leftLength, rightLength, PACKAGE="tractor.track")
        return (StreamlineSource$new(tempFile))
    },
    
    getFileStem = function () { return (file) },
    
    getLengths = function ()
    {
        return (.Call("trkLengths", file, selection, PACKAGE="tractor.track"))
    },
    
    getMapAndLengthData = function ()
    {
        return (.Call("trkFastMapAndLengths", file, selection, labelsPtr., PACKAGE="tractor.track"))
    },
    
    getMedian = function (quantile = 0.99, pathOnly = FALSE)
    {
        tempFile <- threadSafeTempFile()
        .Call("trkMedian", file, selection, tempFile, quantile, PACKAGE="tractor.track")
        
        if (pathOnly)
            return (tempFile)
        else
            return (StreamlineSource$new(tempFile)$getStreamlines())
    },
    
    getSelection = function () { return (selection) },
    
    getStreamlines = function (simplify = TRUE)
    {
        .self$apply(fx(x), simplify=simplify)
    },
    
    getVisitationMap = function (reference = NULL, scope = c("full","seed","ends"), normalise = FALSE)
    {
        scope <- match.arg(scope)
        
        if (is(reference, "MriImage"))
        {
            if (reference$isInternal())
            {
                fileName <- threadSafeTempFile()
                writeImageFile(reference, fileName)
                reference <- fileName
            }
            else
                reference <- reference$getSource()
        }
        else if (!is.character(reference))
            report(OL$Error, "A reference image or path must be provided")
        
        resultFile <- threadSafeTempFile()
        .Call("trkMap", file, selection, reference, scope, normalise, resultFile, PACKAGE="tractor.track")
        
        return (readImageFile(resultFile))
    },
    
    nStreamlines = function () { return (count.) },
    
    select = function (indices = NULL, labels = NULL)
    {
        if (is.null(indices) && is.null(labels))
            .self$selection <- integer(0)
        else
        {
            if (is.null(indices))
                indices <- .Call("trkFind", file, as.integer(labels), labelsPtr., PACKAGE="tractor.track")
            .self$selection <- as.integer(indices)
        }
        
        invisible(.self)
    },
    
    summarise = function ()
    {
        properties <- .Call("trkInfo", file, PACKAGE="tractor.track")
        values <- c("Number of streamlines"=count., "Streamline properties"=implode(properties,sep=", "), "Auxiliary label file"=!is.null(labelsPtr.))
        return (values)
    }
))

setClassUnion("MriImageOrNull", c("MriImage","NULL"))

StreamlineSink <- setRefClass("StreamlineSink", fields=list(file="character",mask="MriImageOrNull",sinkPtr.="ExternalPointerOrNull"), methods=list(
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
