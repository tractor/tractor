Streamline <- setRefClass("Streamline", fields=list(line="matrix",seedIndex="integer",originalSeedPoint="numeric",coordUnit="character",pointSpacings="numeric"), methods=list(
    initialize = function (line = matrix(), seedIndex = NULL, originalSeedPoint = NULL, coordUnit = c("vox","mm"), ...)
    {
        coordUnit <- match.arg(coordUnit)
        
        object <- initFields(line=promote(line,byrow=TRUE), seedIndex=as.integer(seedIndex), originalSeedPoint=as.numeric(originalSeedPoint), coordUnit=coordUnit)

        if (ncol(object$line) != 3)
            report(OL$Error, "Streamline must be specified as a matrix with 3 columns")
        
        if (nrow(object$line) == 1)
            object$pointSpacings <- numeric(0)
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
        
        count <- as.integer(.Call("trkCount", ensureFileSuffix(file,NULL,strip="trk"), PACKAGE="tractor.track"))
        
        return (initFields(file=file, count.=count))
    },
    
    getMedian = function (quantile = 0.99)
    {
        
    },
    
    nStreamlines = function () { return (count.) }
))
