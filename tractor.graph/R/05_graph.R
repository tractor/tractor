Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexNames="character",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeNames="character",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexNames = NULL, vertexLocations = matrix(NA,0,0), locationUnit = "", edges = matrix(NA,0,0), edgeNames = character(0), edgeWeights = rep(1,nrow(edges)), directed = FALSE)
    {
        return (initFields(vertexCount=as.integer(vertexCount), vertexNames=as.character(vertexNames), vertexLocations=vertexLocations, locationUnit=locationUnit, edges=edges, edgeNames=edgeNames, edgeWeights=as.numeric(edgeWeights), directed=directed))
    },
    
    getConnectedVertices = function () { return (sort(unique(as.vector(edges)))) },
    
    getConnectionMatrix = function ()
    {
        connectionMatrix <- matrix(0, nrow=vertexCount, ncol=vertexCount)
        if (!is.null(vertexNames))
        {
            rownames(connectionMatrix) <- vertexNames
            colnames(connectionMatrix) <- vertexNames
        }
        connectionMatrix[edges] <- edgeWeights
        if (!directed)
            connectionMatrix[edges[,2:1]] <- edgeWeights
        return (connectionMatrix)
    },
        
    getEdge = function (i)
    {
        if (i < 0 || i > .self$nEdges())
            return (NA)
        else
            return (edges[i,])
    },
    
    getEdges = function () { return (edges) },
    
    getEdgeNames = function () { return (edgeNames) },
    
    getEdgeWeights = function () { return (edgeWeights) },
    
    getVertexDegree = function () { return (table(factor(edges, levels=1:vertexCount))) },
    
    getVertexLocations = function () { return (vertexLocations) },
    
    getVertexLocationUnit = function () { return (locationUnit) },
    
    getVertexNames = function () { return (vertexNames) },
    
    isDirected = function () { return (directed) },
    
    nEdges = function () { return (nrow(edges)) },
    
    nVertices = function () { return (vertexCount) },
    
    setVertexLocations = function (locs, unit)
    {
        .self$vertexLocations <- locs
        .self$locationUnit <- unit
    }
))

setAs("Graph", "igraph", function (from) {
    require(igraph)
    return (graph.edgelist(from$getEdges(), directed=from$isDirected()))
})
    
setMethod("plot", "Graph", function(x, y, col = "grey60", cex = 1, lwd = 2, radius = NULL, add = FALSE, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, useAlpha = FALSE, hideDisconnected = FALSE, useLocations = FALSE, locationAxes = NULL) {
    edges <- x$getEdges()
    weights <- x$getEdgeWeights()
    
    if (all(is.na(weights)))
        weights <- rep(1, length(weights))
    
    if (useAbsoluteWeights)
        weights <- abs(weights)
    
    if (is.null(weightLimits))
        weightLimits <- range(weights)
    else if (ignoreBeyondLimits)
        weights[weights < weightLimits[1] | weights > weightLimits[2]] <- NA
    else
    {
        weights[weights < weightLimits[1]] <- weightLimits[1]
        weights[weights > weightLimits[2]] <- weightLimits[2]
    }
    
    absWeights <- abs(weights)
    
    if (is.numeric(col))
        col <- tractor.base:::getColourScale(col)$colours
    
    if (length(col) > 1)
    {
        colourIndices <- round(((weights - weightLimits[1]) / (weightLimits[2] - weightLimits[1])) * (length(col)-1)) + 1
        colours <- col[colourIndices]
    }
    else
        colours <- rep(col, length(weights))
    
    if (useAlpha)
    {
        absWeightLimits <- c(max(0,min(weightLimits,absWeights,na.rm=TRUE)), max(abs(weightLimits),absWeights,na.rm=TRUE))
        alphaValues <- round(((absWeights - absWeightLimits[1]) / (absWeightLimits[2] - absWeightLimits[1])) * 255)
        rgbColours <- col2rgb(colours)
        colours[!is.na(colours)] <- sapply(which(!is.na(colours)), function (i) sprintf("#%02X%02X%02X%02X",rgbColours[1,i],rgbColours[2,i],rgbColours[3,i],alphaValues[i]))
    }
    
    if (hideDisconnected)
        activeVertices <- x$getConnectedVertices()
    else
        activeVertices <- 1:x$nVertices()
    nActiveVertices <- length(activeVertices)
    
    if (!is.null(order))
        activeVertices <- order[is.element(order,activeVertices)]
    
    from <- match(edges[!is.na(weights),1], activeVertices)
    to <- match(edges[!is.na(weights),2], activeVertices)
    colours <- colours[!is.na(weights)]
    
    if (useLocations)
    {
        if (is.null(locationAxes) || length(locationAxes) != 2)
            report(OL$Error, "Location axes must be specified, as a vector of two integers")
        
        locations <- x$getVertexLocations()
        xLocs <- locations[activeVertices,locationAxes[1]]
        yLocs <- locations[activeVertices,locationAxes[2]]
        if (any(is.na(xLocs)) || any(is.na(yLocs)))
            report(OL$Error, "Some locations are missing")
        
        xlim <- range(xLocs) + c(-0.1,0.1) * diff(range(xLocs))
        ylim <- range(yLocs) + c(-0.1,0.1) * diff(range(yLocs))
        
        if (is.null(radius))
            radius <- 0.03 * max(diff(range(xLocs)), diff(range(yLocs)))
    }
    else
    {
        angles <- (0:(nActiveVertices-1)) * 2 * pi / nActiveVertices
        xLocs <- sin(angles)
        yLocs <- cos(angles)
        xlim <- ylim <- c(-1.2, 1.2)
        
        if (is.null(radius))
        {
            arcSeparation <- 2 * pi / nActiveVertices
            radius <- min(arcSeparation/4, 0.1)
        }
    }
    
    if (!add)
    {
        oldPars <- par(mai=c(0,0,0,0))
        plot(NA, type="n", xlim=xlim, ylim=ylim)    
    }
    segments(xLocs[from], yLocs[from], xLocs[to], yLocs[to], lwd=lwd, col=colours)
    symbols(xLocs, yLocs, circles=rep(radius,nActiveVertices), inches=FALSE, col="grey50", lwd=lwd, bg="white", add=TRUE)
    text(xLocs, yLocs, as.character(activeVertices), col="grey40", cex=cex)
    if (!add)
        par(oldPars)
})

levelplot.Graph <- function (x, data = NULL, col = 4, cex = 1, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, hideDisconnected = FALSE, ...)
{
    connectionMatrix <- x$getConnectionMatrix()
    edges <- x$getEdges()
    
    if (all(is.na(connectionMatrix)))
        report(OL$Error, "There are no connection weights in the specified graph")
    
    if (useAbsoluteWeights)
        connectionMatrix <- abs(connectionMatrix)
    
    if (is.null(weightLimits))
        weightLimits <- range(connectionMatrix, na.rm=TRUE)
    else if (ignoreBeyondLimits)
        connectionMatrix[connectionMatrix < weightLimits[1] | connectionMatrix > weightLimits[2]] <- NA
    else
    {
        connectionMatrix[connectionMatrix < weightLimits[1]] <- weightLimits[1]
        connectionMatrix[connectionMatrix > weightLimits[2]] <- weightLimits[2]
    }
    
    if (is.numeric(col))
        col <- tractor.base:::getColourScale(col)$colours
    
    if (hideDisconnected)
        activeVertices <- x$getConnectedVertices()
    else
        activeVertices <- 1:x$nVertices()
    nActiveVertices <- length(activeVertices)
    
    if (!is.null(order))
        activeVertices <- order[is.element(order,activeVertices)]
    
    labels <- as.character(activeVertices)
    
    levelplot(connectionMatrix[activeVertices,activeVertices], col.regions=col, at=seq(weightLimits[1],weightLimits[2],length.out=20), scales=list(x=list(labels=labels,tck=0,rot=60,col="grey40",cex=cex), y=list(labels=labels,tck=0,col="grey40",cex=cex)), xlab="", ylab="", ...)
}

newGraphFromTable <- function (table, method = c("correlation","covariance"), allVertexNames = NULL)
{
    method <- match.arg(method)
    
    if (method == "correlation")
        connectionMatrix <- cor(table)
    else if (method == "covariance")
        connectionMatrix <- cov(table)
    
    return (newGraphFromConnectionMatrix(connectionMatrix, directed=FALSE, allVertexNames=allVertexNames))
}

newGraphFromConnectionMatrix <- function (connectionMatrix, directed = FALSE, allVertexNames = NULL, ignoreSelfConnections = FALSE)
{
    if (!is.matrix(connectionMatrix))
        report(OL$Error, "Specified connection matrix is not a matrix object")
    
    if (!directed)
        connectionMatrix[lower.tri(connectionMatrix,diag=FALSE)] <- NA
    if (ignoreSelfConnections)
        diag(connectionMatrix) <- NA
    
    if (is.null(allVertexNames))
        allVertexNames <- union(rownames(connectionMatrix), colnames(connectionMatrix))
    rowVertexLocs <- match(rownames(connectionMatrix), allVertexNames)
    colVertexLocs <- match(colnames(connectionMatrix), allVertexNames)
    
    edges <- which(!is.na(connectionMatrix) & connectionMatrix != 0, arr.ind=TRUE)
    edgeWeights <- connectionMatrix[edges]
    edges[,1] <- rowVertexLocs[edges[,1]]
    edges[,2] <- colVertexLocs[edges[,2]]
    dimnames(edges) <- NULL
    
    return (Graph$new(vertexCount=length(allVertexNames), vertexNames=allVertexNames, edges=edges, edgeWeights=edgeWeights, directed=directed))   
}

newGraphWithVertices <- function (graph, vertices)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    if (length(vertices) == 0)
        report(OL$Error, "At least one vertex must be retained")
    
    vertices <- sort(vertices)
    
    nVertices <- graph$nVertices()
    vertexNames <- graph$getVertexNames()
    if (length(vertexNames) == nVertices)
        vertexNames <- vertexNames[vertices]
    vertexLocations <- graph$getVertexLocations()
    if (nrow(vertexLocations) == nVertices)
        vertexLocations <- vertexLocations[vertices,]
    
    nEdges <- graph$nEdges()
    edges <- graph$getEdges()
    edgesToKeep <- which((edges[,1] %in% vertices) & (edges[,2] %in% vertices))
    edges <- matrix(match(edges[edgesToKeep,],vertices), ncol=2)
    edgeNames <- graph$getEdgeNames()
    if (length(edgeNames) == nEdges)
        edgeNames <- edgeNames[edgesToKeep]
    
    return (Graph$new(vertexCount=length(vertices), vertexNames=vertexNames, vertexLocations=vertexLocations, locationUnit=graph$getVertexLocationUnit(), edges=edges, edgeNames=edgeNames, edgeWeights=graph$getEdgeWeights()[edgesToKeep], directed=graph$isDirected()))
}

newGraphWithEdgeWeightThreshold <- function (graph, threshold, ignoreSign = FALSE, keepUnweighted = TRUE)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
    edgeWeights <- graph$getEdgeWeights()
    
    if (ignoreSign)
        toKeep <- which(abs(edgeWeights) >= threshold)
    else
        toKeep <- which(edgeWeights >= threshold)
    
    if (keepUnweighted)
        toKeep <- c(toKeep, which(is.na(edgeWeights)))
    
    toKeep <- sort(toKeep)
    
    edgeNames <- graph$getEdgeNames()
    if (length(edgeNames) == length(edgeWeights))
        edgeNames <- edgeNames[toKeep]
    
    return (Graph$new(vertexCount=graph$nVertices(), vertexNames=graph$getVertexNames(), vertexLocations=graph$getVertexLocations(), locationUnit=graph$getVertexLocationUnit(), edges=graph$getEdges()[toKeep,,drop=FALSE], edgeNames=edgeNames, edgeWeights=edgeWeights[toKeep], directed=graph$isDirected()))
}
