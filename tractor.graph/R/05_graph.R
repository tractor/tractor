Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexNames="character",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeNames="character",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexNames = NULL, vertexLocations = matrix(NA,0,0), locationUnit = "", edges = matrix(NA,0,0), edgeNames = character(0), edgeWeights = rep(NA,nrow(edges)), directed = FALSE)
    {
        return (initFields(vertexCount=as.integer(vertexCount), vertexNames=as.character(vertexNames), vertexLocations=vertexLocations, locationUnit=locationUnit, edges=edges, edgeNames=edgeNames, edgeWeights=as.numeric(edgeWeights), directed=directed))
    },
    
    getConnectionMatrix = function ()
    {
        connectionMatrix <- matrix(NA, nrow=vertexCount, ncol=vertexCount)
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
    
    getVertexLocations = function () { return (vertexLocations) },
    
    getVertexLocationUnit = function () { return (locationUnit) },
    
    getVertexNames = function () { return (vertexNames) },
    
    isDirected = function () { return (directed) },
    
    nEdges = function () { return (nrow(edges)) },
    
    nVertices = function () { return (vertexCount) }
))

setAs("Graph", "igraph", function (from) {
    require(igraph)
    return (graph.edgelist(from$getEdges(), directed=from$isDirected()))
})
    
setMethod("plot", "Graph", function(x, y, col = "grey60", cex = 1, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, useAlpha = FALSE, hideDisconnected = FALSE) {
    edges <- x$getEdges()
    weights <- x$getEdgeWeights()
    
    if (all(is.na(weights)))
        weights <- rep(1, length(weights))
    
    if (useAbsoluteWeights)
        weights <- abs(Weights)
    
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
    
    if (length(col) > 1)
    {
        colourIndices <- round(((weights - weightLimits[1]) / (weightLimits[2] - weightLimits[1])) * (length(col)-1)) + 1
        colours <- col[colourIndices]
    }
    else
        colours <- rep(col, length(weights))
    
    if (useAlpha)
    {
        absWeightLimits <- c(max(0,min(weightLimits,absWeights,na.rm=TRUE)), max(weightLimits,absWeights,na.rm=TRUE))
        alphaValues <- round(((absWeights - absWeightLimits[1]) / (absWeightLimits[2] - absWeightLimits[1])) * 255)
        rgbColours <- col2rgb(colours)
        colours[!is.na(colours)] <- sapply(which(!is.na(colours)), function (i) sprintf("#%02X%02X%02X%02X",rgbColours[1,i],rgbColours[2,i],rgbColours[3,i],alphaValues[i]))
    }
    
    if (hideDisconnected)
        activeVertices <- sort(union(edges[,1], edges[,2]))
    else
        activeVertices <- 1:x$nVertices()
    nActiveVertices <- length(activeVertices)
    
    if (!is.null(order))
        activeVertices <- order[is.element(order,activeVertices)]
    
    from <- match(edges[!is.na(weights),1], activeVertices)
    to <- match(edges[!is.na(weights),2], activeVertices)
    colours <- colours[!is.na(weights)]
    
    angles <- (0:(nActiveVertices-1)) * 2 * pi / nActiveVertices
    xLocs <- sin(angles)
    yLocs <- cos(angles)
    arcSeparation <- 2 * pi / nActiveVertices
    radius <- min(arcSeparation/4, 0.1)
    
    oldPars <- par(mai=c(0,0,0,0))
    plot(NA, type="n", xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
    segments(xLocs[from], yLocs[from], xLocs[to], yLocs[to], lwd=2, col=colours)
    symbols(xLocs, yLocs, circles=rep(radius,nActiveVertices), inches=FALSE, col="grey50", lwd=2, bg="white", add=TRUE)
    text(xLocs, yLocs, as.character(activeVertices), col="grey40", cex=cex)
    par(oldPars)
})

newGraphFromTable <- function (table, method = c("correlation","covariance"), threshold = NULL, ignoreSign = FALSE, allVertexNames = NULL)
{
    method <- match.arg(method)
    
    if (method == "correlation")
        connectionMatrix <- cor(table)
    else if (method == "covariance")
        connectionMatrix <- cov(table)
    
    return (newGraphFromConnectionMatrix(connectionMatrix, threshold=threshold, ignoreSign=ignoreSign, directed=FALSE, allVertexNames=allVertexNames))
}

newGraphFromConnectionMatrix <- function (connectionMatrix, threshold = NULL, ignoreSign = FALSE, directed = FALSE, allVertexNames = NULL)
{
    if (!is.null(threshold))
    {
        if (ignoreSign)
            connectionMatrix[abs(connectionMatrix) < threshold] <- NA
        else
            connectionMatrix[connectionMatrix < threshold] <- NA
    }
    
    if (!directed)
        connectionMatrix[lower.tri(connectionMatrix,diag=TRUE)] <- NA
    
    if (is.null(allVertexNames))
        allVertexNames <- union(rownames(connectionMatrix), colnames(connectionMatrix))
    rowVertexLocs <- match(rownames(connectionMatrix), allVertexNames)
    colVertexLocs <- match(colnames(connectionMatrix), allVertexNames)
    
    edges <- which(!is.na(connectionMatrix), arr.ind=TRUE)
    edgeWeights <- connectionMatrix[edges]
    edges[,1] <- rowVertexLocs[edges[,1]]
    edges[,2] <- colVertexLocs[edges[,2]]
    dimnames(edges) <- NULL
    
    return (Graph$new(vertexCount=length(allVertexNames), vertexNames=allVertexNames, edges=edges, edgeWeights=edgeWeights, directed=FALSE))   
}
