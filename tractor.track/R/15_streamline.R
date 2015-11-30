Streamline <- setRefClass("Streamline", contains="SerialisableObject", fields=list(line="matrix",seedIndex="integer",voxelDims="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = matrix(), seedIndex = NULL, voxelDims = NULL, coordUnit = c("vox","mm"), pointSpacings = NULL, ...)
    {
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), voxelDims=as.numeric(voxelDims)[1:3], coordUnit=match.arg(coordUnit))

        if (ncol(object$line) != 3)
            report(OL$Error, "Streamline must be specified as a matrix with 3 columns")
        
        if (!is.null(pointSpacings))
            object$pointSpacings <- rep(as.numeric(pointSpacings), length.out=nrow(object$line)-1)
        else if (nrow(object$line) == 1)
            object$pointSpacings <- numeric(0)
        else
            object$pointSpacings <- apply(diff(object$line), 1, vectorLength)
        
        return (object)
    },
    
    getCoordinateUnit = function () { return (coordUnit) },
    
    getLine = function () { return (line) },
    
    getLineLength = function () { return (sum(pointSpacings)) },
    
    getPointSpacings = function () { return (pointSpacings) },
    
    getSeedIndex = function () { return (seedIndex) },
    
    getSeedPoint = function () { return (line[seedIndex,]) },
    
    getVoxelDimensions = function () { return (voxelDims) },
    
    nPoints = function () { return (nrow(line)) },
    
    setCoordinateUnit = function (newUnit = c("vox","mm"))
    {
        newUnit <- match.arg(newUnit)
        if (newUnit == "vox" && .self$coordUnit == "mm")
            .self$line <- t(apply(line-1, 1, "*", abs(voxelDims)))
        else if (newUnit == "mm" && .self$coordUnit == "vox")
            .self$line <- t(apply(line, 1, "/", abs(voxelDims))) + 1
        
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
            rightStop <- ifelse(length(rightWide) > 0, min(rightWide), nPoints)
            
            .self$trim(leftStop, rightStop)
        }
        
        invisible(.self)
    },
    
    transform = function (xfm, reverse = FALSE, ...)
    {
        .self$line <- transformPoints(xfm, .self$line, voxel=(.self$coordUnit=="vox"), reverse=reverse, ...)
        .self$updatePointSpacings()
        
        if (reverse)
            .self$voxelDims <- xfm$getSourceImage()$getVoxelDimensions()[1:3]
        else
            .self$voxelDims <- xfm$getTargetImage()$getVoxelDimensions()[1:3]
        
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
            .self$seedIndex <- seedIndex - start + 1
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

StreamlineSource <- setRefClass("StreamlineSource", fields=list(file="character",count.="integer"), methods=list(
    initialize = function (file = NULL, ...)
    {
        if (is.null(file))
            report(OL$Error, "Streamline source file must be specified")
        if (!file.exists(ensureFileSuffix(file, "trk")))
            report(OL$Error, "Specified streamline source file does not exist")
        
        file <- ensureFileSuffix(file, NULL, strip="trk")
        count <- as.integer(.Call("trkCount", file, PACKAGE="tractor.track"))
        
        return (initFields(file=file, count.=count))
    },
    
    getMedian = function (quantile = 0.99)
    {
        tempFile <- threadSafeTempFile()
        .Call("trkMedian", file, tempFile, quantile, PACKAGE="tractor.track")
        return (StreamlineSource$new(tempFile)$getStreamlines(1L))
    },
    
    getStreamlines = function (indices = NULL, simplify = TRUE)
    {
        if (is.null(indices))
            indices <- seq_len(count.)
        
        streamlines <- vector("list", length(indices))
        i <- 1
        
        .addStreamline <- function (points, seedIndex, voxelDims, coordUnit)
        {
            streamlines[[i]] <<- Streamline$new(points, seedIndex, voxelDims, coordUnit)
            i <<- i + 1
        }
        
        .Call("trkApply", file, indices, .addStreamline, PACKAGE="tractor.track")
        
        if (simplify && length(indices) == 1)
            return (streamlines[[1]])
        else
            return (streamlines)
    },
    
    getVisitationMap = function (indices = NULL, reference = NULL)
    {
        if (is.null(indices))
            indices <- seq_len(count.)
        
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
        .Call("trkMap", file, indices, reference, resultFile, PACKAGE="tractor.track")
        
        return (readImageFile(resultFile))
    },
    
    nStreamlines = function () { return (count.) }
))
