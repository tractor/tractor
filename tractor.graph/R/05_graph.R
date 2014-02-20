Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexAttributes="list",vertexLocations="matrix",locationUnit="character",locationSpace="character",edges="matrix",edgeAttributes="list",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexAttributes = list(), vertexLocations = emptyMatrix(), locationUnit = "", locationSpace = "", edges = emptyMatrix(), edgeAttributes = list(), edgeWeights = rep(1,nrow(edges)), directed = FALSE, ...)
    {
        oldFields <- list(...)
        if ("vertexNames" %in% names(oldFields))
            vertexAttributes <- list(names=as.character(oldFields$vertexNames))
        if ("edgeNames" %in% names(oldFields))
            edgeAttributes <- list(names=as.character(oldFields$edgeNames))
        
        return (initFields(vertexCount=as.integer(vertexCount), vertexAttributes=vertexAttributes, vertexLocations=vertexLocations, locationUnit=locationUnit, locationSpace=locationSpace, edges=edges, edgeAttributes=edgeAttributes, edgeWeights=as.numeric(edgeWeights), directed=directed))
    },
    
    getConnectedVertices = function () { return (sort(unique(as.vector(edges)))) },
    
    getAssociationMatrix = function ()
    {
        associationMatrix <- matrix(0, nrow=vertexCount, ncol=vertexCount)
        if (!is.null(vertexAttributes$names))
        {
            rownames(associationMatrix) <- vertexAttributes$names
            colnames(associationMatrix) <- vertexAttributes$names
        }
        associationMatrix[edges] <- edgeWeights
        if (!directed)
            associationMatrix[edges[,2:1]] <- edgeWeights
        return (associationMatrix)
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
    
    getEdgeDensity = function (disconnectedVertices = FALSE, selfConnections = FALSE)
    {
        if (!disconnectedVertices)
            nConnectedVertices <- length(.self$getConnectedVertices())
        else
            nConnectedVertices <- .self$nVertices()
        
        nEdges <- .self$nEdges() - ifelse(selfConnections, 0, sum(edges[,1]==edges[,2]))
        nPossibleEdges <- ifelse(.self$isDirected(), nConnectedVertices^2, nConnectedVertices*(nConnectedVertices+1)/2) - ifelse(selfConnections, 0, nConnectedVertices)
        
        return (nEdges / nPossibleEdges)
    },
    
    getEdgeWeights = function () { return (edgeWeights) },
    
    getLaplacianMatrix = function ()
    {
        if (directed)
            report(OL$Error, "Laplacian matrix calculation for directed graphs is not yet implemented")
        
        associationMatrix <- .self$getAssociationMatrix()
        degreeMatrix <- diag(colSums(associationMatrix))
        return (degreeMatrix - associationMatrix)
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
    
    getVertexLocationSpace = function () { return (locationSpace) },
    
    isDirected = function () { return (directed) },
    
    isWeighted = function () { return (!all(is.na(edgeWeights) | (edgeWeights %in% c(0,1)))) },
    
    nEdges = function () { return (nrow(edges)) },
    
    nVertices = function () { return (vertexCount) },
    
    setEdgeAttributes = function (...)
    {
        attributes <- c(list(...), edgeAttributes)
        .self$edgeAttributes <- attributes[!duplicated(names(attributes))]
    },
    
    setEdgeWeights = function (newWeights)
    {
        if (length(newWeights) == 1)
            newWeights <- rep(newWeights, .self$nEdges())
        else if (length(newWeights) != .self$nEdges())
        {
            flag(OL$Warning, "Recycling edge weights (length #{length(newWeights)}) to match the number of edges (#{.self$nEdges()})")
            newWeights <- rep(newWeights, length.out=.self$nEdges())
        }
        .self$edgeWeights <- newWeights
    },
    
    setVertexAttributes = function (...)
    {
        attributes <- c(list(...), vertexAttributes)
        .self$vertexAttributes <- attributes[!duplicated(names(attributes))]
    },
    
    setVertexLocations = function (locs, unit, space)
    {
        .self$vertexLocations <- locs
        .self$locationUnit <- unit
        .self$locationSpace <- space
    },
    
    summarise = function ()
    {
        properties <- c(ifelse(.self$isDirected(),"directed","undirected"), ifelse(.self$isWeighted(),"weighted","unweighted"))
        
        vertexAttribNames <- names(vertexAttributes)
        if (length(vertexAttribNames) == 0)
            vertexAttribNames <- "(none)"
        edgeAttribNames <- names(edgeAttributes)
        if (length(edgeAttribNames) == 0)
            edgeAttribNames <- "(none)"
        
        values <- c(implode(properties,sep=", "), .self$nVertices(), .self$nEdges(), s("#{.self$getEdgeDensity()*100}%",round=2), implode(vertexAttribNames,sep=", "), implode(edgeAttribNames,sep=", "))
        names(values) <- c("Graph properties", "Number of vertices", "Number of edges", "Edge density", "Vertex attributes", "Edge attributes")
        return (values)
    }
))

setAs("Graph", "matrix", function (from) from$getAssociationMatrix())

setAs("Graph", "igraph", function (from) {
    require("igraph")

    igraph <- graph.edgelist(from$getEdges(), directed=from$isDirected())
    vertexCountDifference <- from$nVertices() - vcount(igraph)
    if (vertexCountDifference > 0)
        igraph <- add.vertices(igraph, vertexCountDifference)
    
    vertexAttributes <- from$getVertexAttributes()
    edgeAttributes <- from$getEdgeAttributes()
    
    for (i in seq_along(vertexAttributes))
    {
        indices <- which(!is.na(vertexAttributes[[i]]))
        if (names(vertexAttributes)[i] == "names")
            V(igraph)$name[indices] <- vertexAttributes[[i]]
        else
            igraph <- set.vertex.attribute(igraph, names(vertexAttributes)[i], indices, vertexAttributes[[i]][indices])
    }
    
    for (i in seq_along(edgeAttributes))
    {
        indices <- which(!is.na(edgeAttributes[[i]]))
        if (names(edgeAttributes)[i] == "names")
            E(igraph)$name[indices] <- edgeAttributes[[i]]
        else
            igraph <- set.edge.attribute(igraph, names(edgeAttributes)[i], indices, edgeAttributes[[i]][indices])
    }
    
    if (from$isWeighted())
    {
        indices <- which(!is.na(from$getEdgeWeights()))
        E(igraph)$weight[indices] <- from$getEdgeWeights()[indices]
    }
    
    return (igraph)
})

setAs("matrix", "Graph", function (from) asGraph(from))

asGraph <- function (x, ...)
{
    UseMethod("asGraph")
}

asGraph.matrix <- function (x, edgeList = NULL, directed = NULL, selfConnections = TRUE, allVertexNames = NULL, ...)
{
    if (is.null(edgeList))
        edgeList <- (ncol(x) == 2)
    
    if (edgeList)
    {
        if (is.null(directed))
            report(OL$Error, "Directedness must be specified when creating a graph from an edge list")
        
        nVertices <- max(x, length(allVertexNames))
        edges <- structure(x, dimnames=NULL)
        
        if (!directed)
        {
            toSwitch <- (edges[,1] > edges[,2])
            edges[toSwitch,] <- edges[toSwitch,2:1]
            edges <- edges[!duplicated(edges),,drop=FALSE]
        }
        if (!selfConnections)
        {
            toDrop <- (edges[,1] == edges[,2])
            edges <- edges[!toDrop,,drop=FALSE]
        }
        
        edgeWeights <- rep(1, nrow(edges))
    }
    else
    {
        if (ncol(x) != nrow(x))
            report(OL$Error, "Association matrix must be square")
        else if (is.null(directed))
            directed <- !isSymmetric(x)
        else if (directed == isSymmetric(x))
            flag(OL$Warning, "The \"directed\" argument does not match the symmetry of the matrix")
    
        if (!directed)
            x[lower.tri(x,diag=FALSE)] <- NA
        if (!selfConnections)
            diag(x) <- NA
    
        if (is.null(allVertexNames))
            allVertexNames <- union(rownames(x), colnames(x))
        
        nVertices <- max(ncol(x), length(allVertexNames))
    
        if (is.null(allVertexNames))
        {
            rowVertexLocs <- 1:nrow(x)
            colVertexLocs <- 1:ncol(x)
        }
        else
        {
            rowVertexLocs <- match(rownames(x), allVertexNames)
            colVertexLocs <- match(colnames(x), allVertexNames)
        }
    
        edges <- which(!is.na(x) & x != 0, arr.ind=TRUE)
        edgeWeights <- x[edges]
        edges[,1] <- rowVertexLocs[edges[,1]]
        edges[,2] <- colVertexLocs[edges[,2]]
        dimnames(edges) <- NULL
    }
    
    return (Graph$new(vertexCount=nVertices, vertexAttributes=list(names=allVertexNames), edges=edges, edgeWeights=edgeWeights, directed=directed))
}

as.matrix.Graph <- function (x, ...)
{
    as(x, "matrix")
}

setMethod("[", signature(x="Graph",i="missing",j="missing"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[,,drop=drop]))
setMethod("[", signature(x="Graph",i="ANY",j="missing"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[i,,drop=drop]))
setMethod("[", signature(x="Graph",i="missing",j="ANY"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[,j,drop=drop]))
setMethod("[", signature(x="Graph",i="ANY",j="ANY"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[i,j,drop=drop]))
    
setMethod("plot", "Graph", function(x, y, col = NULL, cex = NULL, lwd = 2, radius = NULL, add = FALSE, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, useAlpha = FALSE, hideDisconnected = FALSE, useNames = FALSE, useLocations = FALSE, locationAxes = NULL) {
    edges <- x$getEdges()
    weights <- x$getEdgeWeights()
    
    if (all(is.na(weights)))
        weights <- rep(1, length(weights))
    
    if (useAbsoluteWeights)
        weights <- abs(weights)
    
    if (is.null(weightLimits))
    {
        weightLimits <- range(weights, na.rm=TRUE)
        if (weightLimits[1] < 0 && weightLimits[2] > 0)
            weightLimits <- max(abs(weightLimits)) * c(-1,1)
        else
            weightLimits[which.min(abs(weightLimits))] <- 0
        
        report(OL$Info, s("Setting weight limits of #{weightLimits[1]} to #{weightLimits[2]}",signif=4))
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
        col <- getColourScale(col)$colours
    
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
    
    if (is.null(cex))
        cex <- 50 / nActiveVertices
    
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
        plot(NA, type="n", xlim=xlim, ylim=ylim, asp=1, axes=FALSE)
    }
    segments(xLocs[from], yLocs[from], xLocs[to], yLocs[to], lwd=lwd, col=colours)
    symbols(xLocs, yLocs, circles=rep(radius,nActiveVertices), inches=FALSE, col="grey50", lwd=lwd, bg="white", add=TRUE)
    
    if (useNames)
        text(xLocs, yLocs, x$getVertexAttributes("names")[activeVertices], col="grey40", cex=cex)
    else
        text(xLocs, yLocs, as.character(activeVertices), col="grey40", cex=cex)
    
    if (!add)
        par(oldPars)
})

levelplot.Graph <- function (x, data = NULL, col = NULL, cex = NULL, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, hideDisconnected = FALSE, useNames = FALSE, ...)
{
    associationMatrix <- x$getAssociationMatrix()
    edges <- x$getEdges()
    
    if (all(is.na(associationMatrix)))
        report(OL$Error, "There are no connection weights in the specified graph")
    
    if (useAbsoluteWeights)
        associationMatrix <- abs(associationMatrix)
    
    if (is.null(weightLimits))
    {
        weightLimits <- range(associationMatrix, na.rm=TRUE)
        if (weightLimits[1] < 0 && weightLimits[2] > 0)
            weightLimits <- max(abs(weightLimits)) * c(-1,1)
        else
            weightLimits[which.min(abs(weightLimits))] <- 0
        
        report(OL$Info, s("Setting weight limits of #{weightLimits[1]} to #{weightLimits[2]}",signif=4))
    }
    else if (ignoreBeyondLimits)
        associationMatrix[associationMatrix < weightLimits[1] | associationMatrix > weightLimits[2]] <- NA
    else
    {
        associationMatrix[associationMatrix < weightLimits[1]] <- weightLimits[1]
        associationMatrix[associationMatrix > weightLimits[2]] <- weightLimits[2]
    }
    
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
        col <- getColourScale(col)$colours
    
    if (hideDisconnected)
        activeVertices <- x$getConnectedVertices()
    else
        activeVertices <- 1:x$nVertices()
    nActiveVertices <- length(activeVertices)
    
    if (is.null(cex))
        cex <- 30 / nActiveVertices
    
    if (!is.null(order))
        activeVertices <- order[is.element(order,activeVertices)]
    
    if (useNames)
        labels <- x$getVertexAttributes("names")[activeVertices]
    else
        labels <- as.character(activeVertices)
    
    levelplot(associationMatrix[activeVertices,activeVertices], col.regions=col, at=seq(weightLimits[1],weightLimits[2],length.out=20), scales=list(x=list(labels=labels,tck=0,rot=60,col="grey40",cex=cex), y=list(labels=labels,tck=0,col="grey40",cex=cex)), xlab="", ylab="", ...)
}

inducedSubgraph <- function (graph, vertices)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    if (length(vertices) == 0)
        report(OL$Error, "At least one vertex must be retained")
    
    if (is.character(vertices))
        vertices <- match(vertices, graph$getVertexAttributes("names"))
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

thresholdEdges <- function (graph, threshold, ignoreSign = FALSE, keepUnweighted = TRUE, binarise = FALSE)
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
    
    if (binarise)
        finalEdgeWeights <- rep(1, length(toKeep))
    else
        finalEdgeWeights <- edgeWeights[toKeep]
    
    return (Graph$new(vertexCount=graph$nVertices(), vertexAttributes=graph$getVertexAttributes(), vertexLocations=graph$getVertexLocations(), locationUnit=graph$getVertexLocationUnit(), edges=graph$getEdges()[toKeep,,drop=FALSE], edgeAttributes=edgeAttributes, edgeWeights=finalEdgeWeights, directed=graph$isDirected()))
}

graphEfficiency <- function (graph, type = c("global","local"))
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
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

meanShortestPath <- function (graph, ignoreInfinite = TRUE)
{
    paths <- shortestPaths(graph)
    if (ignoreInfinite)
        paths[is.infinite(paths)] <- NA
    return (mean(paths[upper.tri(paths)], na.rm=TRUE))
}

shortestPaths <- function (graph)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
    require("igraph")
    return (shortest.paths(as(graph,"igraph")))
}

clusteringCoefficients <- function (graph)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
    require("igraph")
    return (transitivity(as(graph,"igraph"),"local"))
}
