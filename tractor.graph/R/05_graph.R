.graphPointer <- function (graph)
{
    graph <- asGraph(graph)
    .Call("graphPointer", graph$nVertices(), graph$getEdges(), graph$getEdgeWeights(), graph$isDirected(), PACKAGE="tractor.graph")
}

#' The Graph class
#' 
#' This class represents a graph, composed of vertices and edges with optional
#' attributes. It can be coerced to an \code{"igraph"} object (from the package
#' of the same name), and to or from a standard numeric matrix.
#' 
#' @field vertexCount The number of vertices in the graph.
#' @field vertexAttributes A named list of attributes which apply to the
#'   vertices. Each component should be a vector of length equal to
#'   \code{vertexCount}.
#' @field vertexLocations A numeric matrix giving the locations of each vertex
#'   in some layout, one per row, or the \code{\link{emptyMatrix}}.
#' @field locationUnit A string giving the units for \code{vertexLocations}, or
#'   the empty string.
#' @field locationSpace A string giving the space for \code{vertexLocations},
#'   or the empty string. This generally only applies to graphs derived from
#'   medical imaging.
#' @field edges An integer matrix listing the vertex indices linked by each
#'   edge, one per line. There should be exactly two columns. If the graph is
#'   undirected, it is not necessary for both directions of each edge to be
#'   stored explicitly.
#' @field edgeAttributes A named list of attributes which apply to the edges.
#'   Each component should be a vector of length equal to the number of rows in
#'   \code{edges}.
#' @field edgeWeights A numeric vector of edge weights, of length equal to the
#'   number of rows in \code{edges}.
#' @field directed Logical value: \code{TRUE} if the graph is directed (so that
#'   edge direction matters); \code{FALSE} otherwise.
#' 
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @export Graph
#' @exportClass Graph
Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexAttributes="list",vertexLocations="matrix",locationUnit="character",locationSpace="character",edges="matrix",edgeAttributes="list",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexAttributes = list(), vertexLocations = emptyMatrix(), locationUnit = "", locationSpace = "", edges = matrix(NA,0,2), edgeAttributes = list(), edgeWeights = rep(1,nrow(edges)), directed = FALSE, ...)
    {
        object <- initFields(vertexCount=as.integer(vertexCount), vertexAttributes=vertexAttributes, vertexLocations=vertexLocations, locationUnit=locationUnit, locationSpace=locationSpace, edges=edges, edgeAttributes=edgeAttributes, edgeWeights=as.numeric(edgeWeights), directed=directed)
        
        # Backwards compatibility
        oldFields <- list(...)
        if ("vertexNames" %in% names(oldFields))
            object$vertexAttributes <- list(name=as.character(oldFields$vertexNames))
        if ("edgeNames" %in% names(oldFields))
            object$edgeAttributes <- list(name=as.character(oldFields$edgeNames))
        if ("names" %in% names(vertexAttributes))
        {
            index <- match("names", names(vertexAttributes))
            names(object$vertexAttributes)[index] <- "name"
        }
        if ("names" %in% names(edgeAttributes))
        {
            index <- match("names", names(edgeAttributes))
            names(object$edgeAttributes)[index] <- "name"
        }
        
        return (object)
    },
    
    binarise = function ()
    {
        "Binarise the graph, setting all nonzero edge weights to 1."
        .self$map(function(x) ifelse(x==0, 0L, 1L))
    },
    
    getAdjacencyMatrix = function ()
    {
        "Obtain the adjacency matrix of the graph, a binarised version of the association matrix whose elements indicate whether or not an edge exists between each pair of vertices."
        return (ifelse(.self$getAssociationMatrix()==0, 0L, 1L))
    },
    
    getAssociationMatrix = function ()
    {
        "Obtain the graph's association matrix, a representation which encapsulates connectivity and edge weights. The matrix will be symmetric for undirected graphs; otherwise rows represent the vertices at the source of each edge and columns the targets."
        associationMatrix <- matrix(0, nrow=vertexCount, ncol=vertexCount)
        if (!is.null(vertexAttributes$name))
            dimnames(associationMatrix) <- list(vertexAttributes$name, vertexAttributes$name)
        associationMatrix[edges] <- edgeWeights
        if (!directed)
            associationMatrix[edges[,2:1]] <- edgeWeights
        return (associationMatrix)
    },
    
    getConnectedVertices = function ()
    {
        "Retrieve a vector of the indices of vertices which are connected by one or more edges."
        return (sort(unique(as.vector(edges))))
    },
    
    getEdges = function (expr)
    {
        "Retrieve the matrix of edges, optionally limiting the result to those selected by the expression supplied."
        if (missing(expr))
            return (edges)
        result <- eval(substitute(expr), edgeAttributes, parent.frame())
        return (edges[result,,drop=FALSE])
    },
    
    getEdgeAttributes = function (attributes = NULL)
    {
        "Retrieve edge attributes. By default the full list is returned, but a subset or a single attribute vector can be selected by name."
        indexList(edgeAttributes, attributes)
    },
    
    getEdgeWeights = function ()
    {
        "Retrieve the vector of current edge weights."
        return (edgeWeights)
    },
    
    getVertexAttributes = function (attributes = NULL)
    {
        "Retrieve vertex attributes. By default the full list is returned, but a subset or a single attribute vector can be selected by name."
        indexList(vertexAttributes, attributes)
    },
    
    getVertexLocations = function ()
    {
        "Retrieve the vertex locations, if available. The units and space of the location information are returned in attributes."
        locs <- vertexLocations
        if (locationUnit != "")
            attr(locs, "unit") <- locationUnit
        if (locationSpace != "")
            attr(locs, "space") <- locationSpace
        return (locs)
    },
    
    getVertexLocationUnit = function () { return (locationUnit) },
    
    getVertexLocationSpace = function () { return (locationSpace) },
    
    getVertices = function (expr)
    {
        "Retrieve the indices of vertices selected by the expression supplied, which is evaluated in an environment in which the vertex attributes are available by name."
        if (missing(expr))
            return (seq_len(vertexCount))
        result <- eval(substitute(expr), vertexAttributes, parent.frame())
        if (is.logical(result) && length(result) == vertexCount)
            return (which(result))
        else
            return (sort(as.integer(result)))
    },
    
    isDirected = function () { return (directed) },
    
    isSelfConnected = function ()
    {
        "Return TRUE if any edge connects a vertex to itself."
        if (nrow(edges) == 0)
            return (FALSE)
        else
            return (any(edges[,1] == edges[,2]))
    },
    
    isWeighted = function ()
    {
        "Return TRUE if any edge weight is neither missing nor 0 or 1."
        return (!all(is.na(edgeWeights) | (edgeWeights %in% c(0,1))))
    },
    
    map = function (fun, ..., matchEdges = FALSE)
    {
        "Modify the graph in-place, replacing the association matrix with the result of applying a function to the current one."
        fun <- match.fun(fun)
        originalMatrix <- .self$getAssociationMatrix()
        modifiedMatrix <- fun(originalMatrix, ...)
        if (equivalent(dim(originalMatrix), dim(modifiedMatrix)))
            .self$setAssociationMatrix(modifiedMatrix, matchEdges=matchEdges)
        invisible (.self)
    },
    
    nEdges = function () { return (nrow(edges)) },
    
    nVertices = function () { return (vertexCount) },
    
    setAssociationMatrix = function (newMatrix, matchEdges = FALSE)
    {
        "Modify the graph in-place, replacing its connectivity using the supplied association matrix (which must have the appropriate dimensions). This will not change the directedness of the graph. If matchEdges is TRUE, attributes other than weight will be carried over for edges that still exist after the operation."
        if (nrow(newMatrix) != .self$nVertices() || ncol(newMatrix) != .self$nVertices())
            report(OL$Error, "Association matrix size does not match vertex count")
        
        if (!.self$isDirected())
            newMatrix[lower.tri(newMatrix,diag=FALSE)] <- NA
        newEdges <- which(!is.na(newMatrix) & newMatrix != 0, arr.ind=TRUE)
        
        if (matchEdges)
        {
            indices <- match(apply(newEdges,1,implode,sep=","), apply(edges,1,implode,sep=","))
            .self$edgeAttributes <- lapply(edgeAttributes, function (attrib) {
                if (length(attrib) == nrow(edges))
                    return (attrib[indices])
                else
                    return (attrib)
            })
        }
        else
            .self$edgeAttributes <- list()
        
        .self$edges <- newEdges
        .self$edgeWeights <- newMatrix[newEdges]
        invisible(.self)
    },
    
    setEdgeAttributes = function (...)
    {
        "Add or replace edge attributes."
        newAttributes <- lapply(list(...), function(x) {
            if (length(x) == 1)
                return (rep(x, .self$nEdges()))
            else if (length(x) != .self$nEdges())
            {
                flag(OL$Warning, "Recycling edge attribute (length #{length(x)}) to match the number of edges (#{.self$nEdges()})")
                return (rep(x, length.out=.self$nEdges()))
            }
            else
                return (x)
        })
        newAttributes <- deduplicate(newAttributes, .self$edgeAttributes)
        .self$edgeAttributes <- newAttributes[!sapply(newAttributes,is.null)]
        invisible(.self)
    },
    
    setEdgeWeights = function (newWeights)
    {
        "Set or replace edge weights."
        if (length(newWeights) == 1)
            newWeights <- rep(newWeights, .self$nEdges())
        else if (length(newWeights) != .self$nEdges())
        {
            flag(OL$Warning, "Recycling edge weights (length #{length(newWeights)}) to match the number of edges (#{.self$nEdges()})")
            newWeights <- rep(newWeights, length.out=.self$nEdges())
        }
        .self$edgeWeights <- newWeights
        invisible(.self)
    },
    
    setVertexAttributes = function (...)
    {
        "Add or replace vertex attributes."
        newAttributes <- lapply(list(...), function(x) {
            if (length(x) == 1)
                return (rep(x, .self$nVertices()))
            else if (length(x) != .self$nVertices())
            {
                flag(OL$Warning, "Recycling vertex attribute (length #{length(x)}) to match the number of vertices (#{.self$nVertices()})")
                return (rep(x, length.out=.self$nVertices()))
            }
            else
                return (x)
        })
        newAttributes <- deduplicate(newAttributes, .self$vertexAttributes)
        .self$vertexAttributes <- newAttributes[!sapply(newAttributes,is.null)]
        invisible(.self)
    },
    
    setVertexLocations = function (locs, unit, space)
    {
        "Set or replace vertex locations. Attributes of the first argument will be used as fallback values for the second and third, if present."
        .self$vertexLocations <- locs
        if (missing(unit) && !is.null(attr(locs,"unit")))
            .self$locationUnit <- attr(locs, "unit")
        else
            .self$locationUnit <- unit
        if (missing(space) && !is.null(attr(locs,"space")))
            .self$locationSpace <- attr(locs, "space")
        else
            .self$locationSpace <- space
        
        invisible(.self)
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
        edgeDensity <- edgeDensity(.self)
        
        values <- c("Graph properties"=implode(properties,sep=", "),
                    "Number of vertices"=.self$nVertices(),
                    "Number of edges"=.self$nEdges(),
                    "Edge density"=ifelse(is.na(edgeDensity), "N/A", es("#{edgeDensity*100}%",round=2)),
                    "Vertex attributes"=implode(vertexAttribNames,sep=", "),
                    "Edge attributes"=implode(edgeAttribNames,sep=", "))
        return (values)
    }
))

setAs("Graph", "matrix", function (from) from$getAssociationMatrix())

setAs("Graph", "igraph", function (from) {
    igraph <- igraph::graph.edgelist(from$getEdges(), directed=from$isDirected())
    vertexCountDifference <- from$nVertices() - igraph::vcount(igraph)
    if (vertexCountDifference > 0)
        igraph <- igraph::add.vertices(igraph, vertexCountDifference)
    
    vertexAttributes <- from$getVertexAttributes()
    edgeAttributes <- from$getEdgeAttributes()
    
    for (i in seq_along(vertexAttributes))
    {
        indices <- which(!is.na(vertexAttributes[[i]]))
        igraph <- igraph::set.vertex.attribute(igraph, names(vertexAttributes)[i], indices, vertexAttributes[[i]][indices])
    }
    
    for (i in seq_along(edgeAttributes))
    {
        indices <- which(!is.na(edgeAttributes[[i]]))
        igraph <- igraph::set.edge.attribute(igraph, names(edgeAttributes)[i], indices, edgeAttributes[[i]][indices])
    }
    
    if (from$isWeighted())
    {
        indices <- which(!is.na(from$getEdgeWeights()))
        igraph::E(igraph)$weight[indices] <- from$getEdgeWeights()[indices]
    }
    
    return (igraph)
})

setAs("matrix", "Graph", function (from) asGraph(from))

#' @export
asGraph <- function (x, ...)
{
    UseMethod("asGraph")
}

#' @export
asGraph.Graph <- function (x, ...)
{
    return (x)
}

#' @export
asGraph.matrix <- function (x, edgeList = NULL, directed = NULL, selfConnections = TRUE, nVertices = NULL, ...)
{
    if (is.null(edgeList))
        edgeList <- (ncol(x) == 2)
    
    vertexAttributes <- list()
    
    if (edgeList)
    {
        if (is.null(directed))
            report(OL$Error, "Directedness must be specified when creating a graph from an edge list")
        
        if (is.null(nVertices))
            nVertices <- max(x)
        else if (max(x) > nVertices)
            report(OL$Error, "At least one edge connects a nonexistent vertex")
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
        
        if (is.null(directed))
            directed <- !isSymmetric(unname(x))
        else if (directed == isSymmetric(unname(x)))
            flag(OL$Warning, "The \"directed\" argument does not match the symmetry of the matrix")
        
        if (is.null(nVertices))
            nVertices <- ncol(x)
        else if (ncol(x) != nVertices)
            report(OL$Error, "The association matrix size does not match the number of vertices")
    
        if (!directed)
            x[lower.tri(x,diag=FALSE)] <- NA
        if (!selfConnections)
            diag(x) <- NA
    
        edges <- which(!is.na(x) & x != 0, arr.ind=TRUE)
        edgeWeights <- x[edges]
        dimnames(edges) <- NULL
        
        if (!is.null(colnames(x)) && !is.null(rownames(x)) && all(colnames(x) == rownames(x)))
            vertexAttributes$name <- colnames(x)
    }
    
    graph <- Graph$new(vertexCount=nVertices, edges=edges, edgeWeights=edgeWeights, directed=directed, vertexAttributes=vertexAttributes)
    
    return (graph)
}

#' @export
as.matrix.Graph <- function (x, ...)
{
    as(x, "matrix")
}

#' @export
dim.Graph <- function (x)
{
    rep(x$nVertices(), 2L)
}

#' @export
setMethod("[", signature(x="Graph",i="missing",j="missing"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[,,drop=drop]))

#' @export
setMethod("[", signature(x="Graph",i="ANY",j="missing"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[i,,drop=drop]))

#' @export
setMethod("[", signature(x="Graph",i="missing",j="ANY"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[,j,drop=drop]))

#' @export
setMethod("[", signature(x="Graph",i="ANY",j="ANY"), function (x, i, j, ..., drop = TRUE) return (x$getAssociationMatrix()[i,j,drop=drop]))

#' @export
setReplaceMethod("[", signature(x="Graph",i="missing",j="missing"), function (x, i, j, ..., value) return (x$map("[<-", value=value)))

#' @export
setReplaceMethod("[", signature(x="Graph",i="ANY",j="missing"), function (x, i, j, ..., value) return (x$map("[<-", i=i, value=value)))

#' @export
setReplaceMethod("[", signature(x="Graph",i="missing",j="ANY"), function (x, i, j, ..., value) return (x$map("[<-", j=j, value=value)))

#' @export
setReplaceMethod("[", signature(x="Graph",i="ANY",j="ANY"), function (x, i, j, ..., value) return (x$map("[<-", i, j, value=value)))

#' @export
setMethod("plot", "Graph", function(x, y, col = NULL, cex = NULL, lwd = 2, radius = NULL, add = FALSE, order = NULL, useAbsoluteWeights = FALSE, weightLimits = NULL, ignoreBeyondLimits = TRUE, useAlpha = FALSE, hideDisconnected = FALSE, useNames = FALSE, useLocations = FALSE, locationAxes = NULL) {
    edges <- x$getEdges()
    weights <- x$getEdgeWeights()
    
    # Ignore self-connection weights, as they won't be visible
    weights[edges[,1] == edges[,2]] <- NA
    
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
        
        report(OL$Info, es("Setting weight limits of #{weightLimits[1]} to #{weightLimits[2]}",signif=4))
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
            col <- -6
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
        cex <- ifelse(useLocations, 0.6, min(1.5,50/nActiveVertices))
    
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
        text(xLocs, yLocs, x$getVertexAttributes("name")[activeVertices], col="grey40", cex=cex)
    else
        text(xLocs, yLocs, as.character(activeVertices), col="grey40", cex=cex)
    
    if (!add)
        par(oldPars)
})

#' @export
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
        
        report(OL$Info, es("Setting weight limits of #{weightLimits[1]} to #{weightLimits[2]}",signif=4))
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
            col <- -6
    }
    if (is.numeric(col))
        col <- getColourScale(col)$colours
    
    if (hideDisconnected)
        activeVertices <- x$getConnectedVertices()
    else
        activeVertices <- 1:x$nVertices()
    nActiveVertices <- length(activeVertices)
    
    if (is.null(cex))
        cex <- min(1.5, 30/nActiveVertices)
    
    if (!is.null(order))
        activeVertices <- order[is.element(order,activeVertices)]
    
    if (useNames)
        labels <- x$getVertexAttributes("name")[activeVertices]
    else
        labels <- as.character(activeVertices)
    
    submatrix <- associationMatrix[activeVertices,activeVertices]
    dimnames(submatrix) <- list(labels, labels)
    
    levelplot(submatrix, col.regions=col, at=seq(weightLimits[1],weightLimits[2],length.out=20), scales=list(x=list(labels=labels,tck=0,rot=60,col="grey40",cex=cex), y=list(labels=labels,tck=0,col="grey40",cex=cex)), xlab="", ylab="", ...)
}

#' @export
inducedSubgraph <- function (graph, vertices = connectedVertices(graph))
{
    graph <- asGraph(graph)
    
    if (length(vertices) == 0)
        report(OL$Error, "At least one vertex must be retained")
    
    if (is.character(vertices))
        vertices <- match(vertices, graph$getVertexAttributes("name"))
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
        vertexLocations <- vertexLocations[vertices,,drop=FALSE]
    
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
    
    return (Graph$new(vertexCount=length(vertices), vertexAttributes=vertexAttributes, vertexLocations=vertexLocations, locationUnit=graph$getVertexLocationUnit(), locationSpace=graph$getVertexLocationSpace(), edges=edges, edgeAttributes=edgeAttributes, edgeWeights=graph$getEdgeWeights()[edgesToKeep], directed=graph$isDirected()))
}

#' @export
thresholdEdges <- function (graph, threshold, ignoreSign = FALSE, keepUnweighted = TRUE, binarise = FALSE)
{
    graph <- asGraph(graph)
    
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
    
    return (Graph$new(vertexCount=graph$nVertices(), vertexAttributes=graph$getVertexAttributes(), vertexLocations=graph$getVertexLocations(), locationUnit=graph$getVertexLocationUnit(), locationSpace=graph$getVertexLocationSpace(), edges=graph$getEdges()[toKeep,,drop=FALSE], edgeAttributes=edgeAttributes, edgeWeights=finalEdgeWeights, directed=graph$isDirected()))
}
