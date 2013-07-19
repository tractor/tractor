Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexAttributes="list",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeAttributes="list",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexAttributes = list(), vertexLocations = matrix(NA,0,0), locationUnit = "", edges = matrix(NA,0,0), edgeAttributes = list(), edgeWeights = rep(1,nrow(edges)), directed = FALSE, ...)
    {
        oldFields <- list(...)
        if ("vertexNames" %in% names(oldFields))
            vertexAttributes <- list(names=as.character(oldFields$vertexNames))
        if ("edgeNames" %in% names(oldFields))
            edgeAttributes <- list(names=as.character(oldFields$edgeNames))
        
        return (initFields(vertexCount=as.integer(vertexCount), vertexAttributes=vertexAttributes, vertexLocations=vertexLocations, locationUnit=locationUnit, edges=edges, edgeAttributes=edgeAttributes, edgeWeights=as.numeric(edgeWeights), directed=directed))
    },
    
    getConnectedVertices = function () { return (sort(unique(as.vector(edges)))) },
    
    getConnectionMatrix = function ()
    {
        connectionMatrix <- matrix(0, nrow=vertexCount, ncol=vertexCount)
        if (!is.null(vertexAttributes$names))
        {
            rownames(connectionMatrix) <- vertexAttributes$names
            colnames(connectionMatrix) <- vertexAttributes$names
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
    
    getEdgeAttributes = function (attributes = NULL)
    {
        if (is.null(attributes))
            return (edgeAttributes)
        else if (length(attributes) == 1)
            return (edgeAttributes[[attributes]])
        else
            return (edgeAttributes[attributes])
    },
    
    getEdgeWeights = function () { return (edgeWeights) },
    
    getLaplacianMatrix = function ()
    {
        if (directed)
            report(OL$Error, "Laplacian matrix calculation for directed graphs is not yet implemented")
        
        connectionMatrix <- .self$getConnectionMatrix()
        degreeMatrix <- diag(colSums(connectionMatrix))
        return (degreeMatrix - connectionMatrix)
    },
    
    getVertexAttributes = function (attributes = NULL)
    {
        if (is.null(attributes))
            return (vertexAttributes)
        else if (length(attributes) == 1)
            return (vertexAttributes[[attributes]])
        else
            return (vertexAttributes[attributes])
    },
    
    getVertexDegree = function () { return (table(factor(edges, levels=1:vertexCount))) },
    
    getVertexLocations = function () { return (vertexLocations) },
    
    getVertexLocationUnit = function () { return (locationUnit) },
    
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
    
setMethod("plot", "Graph", function(x, y, col = NULL, cex = 1, lwd = 2, radius = NULL, add = FALSE, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, useAlpha = FALSE, hideDisconnected = FALSE, useLocations = FALSE, locationAxes = NULL) {
    edges <- x$getEdges()
    weights <- x$getEdgeWeights()
    
    if (all(is.na(weights)))
        weights <- rep(1, length(weights))
    
    if (useAbsoluteWeights)
        weights <- abs(weights)
    
    if (is.null(weightLimits))
    {
        weightLimits <- range(weights)
        if (weightLimits[1] < 0 && weightLimits[2] > 0)
            weightLimits <- max(abs(weightLimits)) * c(-1,1)
        else
            weightLimits[which.min(abs(weightLimits))] <- 0
    }
    else if (ignoreBeyondLimits)
        weights[weights < weightLimits[1] | weights > weightLimits[2]] <- NA
    else
    {
        weights[weights < weightLimits[1]] <- weightLimits[1]
        weights[weights > weightLimits[2]] <- weightLimits[2]
    }
    
    absWeights <- abs(weights)
    
    if (is.null(col))
    {
        if (weightLimits[1] < 0 && weightLimits[2] > 0)
            col <- 4
        else if (weightLimits[1] >= 0 && weightLimits[2] > 0)
            col <- 5
        else if (weightLimits[1] < 0 && weightLimits[2] <= 0)
            col <- 6
    }
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
        plot(NA, type="n", xlim=xlim, ylim=ylim, asp=1)    
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
    
    return (Graph$new(vertexCount=length(allVertexNames), vertexAttributes=list(names=allVertexNames), edges=edges, edgeWeights=edgeWeights, directed=directed))
}

newGraphWithVertices <- function (graph, vertices)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    if (length(vertices) == 0)
        report(OL$Error, "At least one vertex must be retained")
    
    vertices <- sort(vertices)
    
    nVertices <- graph$nVertices()
    vertexAttributes <- lapply(graph$getVertexAttributes(), function (attrib) {
        if (length(attrib) == nVertices)
            return (attrib[vertices])
        else
            return (attrib)
    })
    vertexLocations <- graph$getVertexLocations()
    if (nrow(vertexLocations) == nVertices)
        vertexLocations <- vertexLocations[vertices,]
    
    nEdges <- graph$nEdges()
    edges <- graph$getEdges()
    edgesToKeep <- which((edges[,1] %in% vertices) & (edges[,2] %in% vertices))
    edges <- matrix(match(edges[edgesToKeep,],vertices), ncol=2)
    edgeAttributes <- lapply(graph$getEdgeAttributes(), function (attrib) {
        if (length(attrib) == nEdges)
            return (attrib[edgesToKeep])
        else
            return (attrib)
    })
    
    return (Graph$new(vertexCount=length(vertices), vertexAttributes=vertexAttributes, vertexLocations=vertexLocations, locationUnit=graph$getVertexLocationUnit(), edges=edges, edgeAttributes=edgeAttributes, edgeWeights=graph$getEdgeWeights()[edgesToKeep], directed=graph$isDirected()))
}

newGraphWithEdgeWeightThreshold <- function (graph, threshold, ignoreSign = FALSE, keepUnweighted = TRUE)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
    nEdges <- graph$nEdges()
    edgeWeights <- graph$getEdgeWeights()
    
    if (ignoreSign)
        toKeep <- which(abs(edgeWeights) >= threshold)
    else
        toKeep <- which(edgeWeights >= threshold)
    
    if (keepUnweighted)
        toKeep <- c(toKeep, which(is.na(edgeWeights)))
    
    toKeep <- sort(toKeep)
    
    edgeAttributes <- lapply(graph$getEdgeAttributes(), function (attrib) {
        if (length(attrib) == nEdges)
            return (attrib[toKeep])
        else
            return (attrib)
    })
    
    return (Graph$new(vertexCount=graph$nVertices(), vertexAttributes=graph$getVertexAttributes(), vertexLocations=graph$getVertexLocations(), locationUnit=graph$getVertexLocationUnit(), edges=graph$getEdges()[toKeep,,drop=FALSE], edgeAttributes=edgeAttributes, edgeWeights=edgeWeights[toKeep], directed=graph$isDirected()))
}

calculateMetricsForGraph <- function (graph, metrics = c("density","msp","mcc","globaleff","localeff"))
{
    efficiency <- function (graph, type = c("global","local"))
    {
        require("igraph")
        type <- match.arg(type)
        
        v <- graph$getConnectedVertices()
        if (length(v) < 2)
        {
            if (type == "global")
                return (0)
            else
                return (rep(0,length(v)))
        }
        
        graph <- induced.subgraph(as(graph,"igraph"), v)
        
        if (type == "global")
        {
            sp <- shortest.paths(graph)
            ge <- mean(1/sp[upper.tri(sp) | lower.tri(sp)])
            return (ge)
        }
        else
        {
            # Find neighbourhoods and remove self-connections
            v <- V(graph)
            n <- neighborhood(graph, 1, v)
            n <- lapply(seq_along(n), function(i) setdiff(n[[i]],v[i]))
    
            le <- sapply(n, function (cn) {
                if (length(cn) < 2)
                    return (0)
                else
                {
                    subgraph <- induced.subgraph(graph, cn)
                    sp <- shortest.paths(subgraph)
                    return (mean(1/sp[upper.tri(sp) | lower.tri(sp)]))
                }
            })
            return (le)
        }
    }
    
    metrics <- match.arg(metrics, several.ok=TRUE)
    result <- list()
    
    for (i in seq_along(metrics))
    {
        value <- switch(metrics[i], density={
            nConnectedVertices <- length(graph$getConnectedVertices())
            if (graph$isDirected())
                graph$nEdges() / (nConnectedVertices^2)
            else
                graph$nEdges() / (nConnectedVertices*(nConnectedVertices+1)/2)
        }, msp={
            require("igraph")
            average.path.length(as(graph,"igraph"), directed=graph$isDirected())
        }, mcc={
            require("igraph")
            mean(transitivity(as(graph,"igraph"),"local"), na.rm=TRUE)
        }, globaleff={
            efficiency(graph, "global")
        }, localeff={
            efficiency(graph, "local")
        })
        
        result[[metrics[i]]] <- value
    }
    
    return (result)
}
