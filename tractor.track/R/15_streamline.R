Streamline <- setRefClass("Streamline", fields=list(line="matrix",seedIndex="integer",seedLoc="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = matrix(), seedIndex = NULL, seedLoc = NULL, coordUnit = c("vox","mm"), pointSpacings = NULL, fixedSpacing = NULL, ...)
    {
        coordUnit <- match.arg(coordUnit)
        
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), seedLoc=as.numeric(seedLoc), coordUnit=coordUnit)

        if (ncol(object$line) != 3)
            report(OL$Error, "Streamline must be specified as a matrix with 3 columns")
        
        if (!is.null(pointSpacings))
            object$pointSpacings <- rep(as.numeric(pointSpacings), length.out=nrow(object$line)-1)
        else if (nrow(object$line) == 1)
            object$pointSpacings <- numeric(0)
        else if (isTRUE(fixedSpacing))
            object$pointSpacings <- rep(vectorLength(object$line[2,]-object$line[1,]), nrow(object$line)-1)
        else
            object$pointSpacings <- apply(diff(object$line), 1, vectorLength)
        
        return (object)
    },
    
    getCoordinateUnit = function () { return (coordUnit) },
    
    getLine = function () { return (line) },
    
    getLineLength = function () { return (sum(pointSpacings)) },
    
    getMetadata = function () { return (export("StreamlineTractMetadata")) },
    
    getPointSpacings = function () { return (pointSpacings) },
    
    getSeedIndex = function () { return (seedIndex) },
    
    getOriginalSeedPoint = function () { return (originalSeedPoint) },
    
    getSeedPoint = function () { return (originalSeedPoint) },
    
    getSpatialRange = function ()
    {
        fullRange <- apply(line, 2, range, na.rm=TRUE)
        return (list(mins=fullRange[1,], maxes=fullRange[2,]))
    },
    
    isOriginAtSeed = function () { return (equivalent(line[seedIndex,], c(0,0,0), tolerance=1e-4)) },
    
    nPoints = function () { return (nrow(line)) }
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
        
        .addStreamline <- function (points, seedIndex, seedLoc, coordUnit, fixedSpacing)
        {
            streamlines[[i]] <<- Streamline$new(points, seedIndex, seedLoc, coordUnit, fixedSpacing=fixedSpacing)
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
